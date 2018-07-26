Monday, January 14, 2008
Haskell Shuffling

After reading Jeff Atwood's post about shuffling I decided it would be interesting to implement both his naive shuffling algorithm and the Fisher-Yates algorithm in Haskell. It seemed like a good little exercise and gave me a chance to check out the ST monad and the various Array data types in Haskell.

This post is a literate Haskell program, just copy it into a file named Shuffle.lhs and you can import the Shuffle module into any other Haskell program. Here's the module definition and imports:


> module Shuffle (naiveShuffle, fyShuffle) where
>
> import System.Random
> import Data.Array.ST
> import Data.Array.MArray 
> import Data.Array.IArray 
> import Data.Array.Unboxed 
> import Control.Monad.ST
> import Control.Monad.State

So what were trying to do is take a sequence of numbers and generate a new sequence of numbers that is a random re-ordering of the initial sequence. Since we're going to be dealing with randomly re-ordering elements of a sequence using a list would not be the greatest idea given list's O(n) performance for inserting and deleting elements. Instead we want to use an Array. To be more specific I'm going to use STUArray since I want a mutable array while I'm doing the shuffling. Also since I'm just shuffling Ints I'm using an unboxed array that directly stores the values in the array. Were we shuffling a non-primitive type then we would need to use STArray instead which would cost a bit in performance and memory usage since pointers to the elements are stored in the array instead of the elements themselves. We will also be working in the ST monad, as is required when using mutable arrays outside of the IO monad.

Instead of working in the ST monad we could use IOUArray but then our shuffling routines would only be usable in the IO monad. Using the ST monad gives us a bit more flexibility.

It turns out that the only real difference between the naive and Fisher-Yates(FY) shuffling algorithms is how we choose the elements in the array to swap. In both cases we start with the last element in the array and swap it with a random element from the array, then do the same with the second to last element and so until we get to the start of the array. In the naive algorithm we swap with any element in the array while in the FY algorithm we only consider elements before the current element for swapping. We can encode these rules in the following two functions which pass a bounds filter to the actual shuffle algorithm implementation. The bounds filter function just takes the array bounds and the index of the element being swapped and returns the bounds for generating a random element to swap with:

 
> naiveShuffle :: (Int, Int) -> StdGen -> (UArray Int Int, StdGen)
> naiveShuffle bs randGen = shuffleImpl bs randGen boundsFilter
>     where
>     boundsFilter bs _ = bs
>
> fyShuffle :: (Int, Int) -> StdGen -> (UArray Int Int, StdGen)
> fyShuffle bs randGen = shuffleImpl bs randGen boundsFilter
>     where
>     boundsFilter (lo, _) cur = (lo, cur)
 

Concentrate on boundsFilter for the moment, we'll get to the rest in a minute. boundsFilter takes the array bounds and the index of the current element and generates bounds for the random element index to swap the current element with. In the naive case the bounds for the random index are just the full array bounds while in the FY case the bounds are the lo end of the array bounds and the current index. This is really the only difference between the two algorithms.

The rest of the shuffling algorithm is defined in shuffleImpl which takes the bounds for the array to shuffle, a random number generator and a filter for the bounds. It is implemented as:

 
> shuffleImpl :: (Int, Int) -> StdGen -> 
>                ((Int, Int) -> Int -> (Int, Int)) -> 
>                (UArray Int Int, StdGen)
> shuffleImpl bs@(lo, hi) randGen boundsFilter = runST runShuffle
>     where
>     runShuffle :: ST s (UArray Int Int, StdGen)
>     runShuffle = do a <- createArray bs
>                     r' <- doShuffle hi a randGen
>                     a' <- unsafeFreeze a
>                     return (a', r')
>     doShuffle :: Int -> (STUArray s Int Int) -> StdGen -> 
>                  ST s StdGen
>     doShuffle cur a r
>         | cur> lo = 
>             do
>             (n, r') <- return $ randomR (boundsFilter bs cur) r
>             swapElems a cur n
>             doShuffle (cur - 1) a r'
>         | otherwise = return r
>     swapElems :: (STUArray s Int Int) -> Int -> Int -> ST s ()
>     swapElems a n1 n2 = do
>                         v1 <- readArray a n1
>                         v2 <- readArray a n2
>                         writeArray a n1 v2
>                         writeArray a n2 v1
    

shuffleImpl returns the shuffled array and the updated random number generator. The first thing to note is that we start with a call to runST. We have to use runST instead of runSTUArray because we want to get the updated random number generator out of the ST computation and runSTUArray only returns the computed array. You've probably noticed that there are type annotations on all of the function definitions so far. And so far none of them have been necessary, they're there for pedagogical purposes1. Now for the definition of createArray:

 
> createArray :: (Int, Int) -> (ST s (STUArray s Int Int))
> createArray bs@(low, _) = newListArray bs [low..]
    

When we define createArray the type annotation is required so that the call to MArray.newListArray knows which type of array to create. All newListArray knows is that we want something that is of type-class MArray. The explicit type annotation tells the compiler to use the STUArray instance of MArray when the call to newListArray is made.

So really all shuffleImpl does is use runST to run the runShuffle computation. In runShuffle we use createArray to create a new array initialized to the integers in our bounds in ascending order. Then doShuffle is run which iterates the elements of the array swapping them according to our random number generation scheme. Note that the updates to the random number generator have to be threaded though the calls to doShuffle. When doShuffle is done we have to freeze the mutable array so that it can be sent out of the ST monad and back to the caller of shuffleImpl. We use unsafeFreeze here, which avoids an array copy when the immutable array is created. Since we are not going to use the mutable array anymore beyond this point this is actually a safe thing to do. Finally the immutable array and the updated random number generator are returned.

One thing that gave me trouble when I first started trying to use the ST monad was that I wanted to put forall s . on all my type annotations. The definition of ST involves forall so I thought that I needed forall all over the place as well. The problem is that in all of the ST s types above the compiler fills in s for you. The type for s is hidden in the call to runST and the user of the ST monad does not get to know what it is. It's only purpose is to keep the state of one call to runST separate from any other calls to runST.

Did you notice in doShuffle how we're passing StdGens all over the place? This is screaming out for the State monad, or in our case its cousin the StateT monad transformer. So we're now going to wrap our ST monad in StateT so we don't have to pass random number generators all over the place. We'll call the new version of shuffleImpl that uses StateT shuffleImpl'.

 
> type ShuffleState s a = StateT StdGen (ST s) a
>
> shuffleImpl' :: (Int, Int) -> StdGen -> 
>                 ((Int, Int) -> Int -> (Int, Int)) -> 
>                 (UArray Int Int, StdGen)
> shuffleImpl' bs@(lo, hi) randGen boundsFilter = 
>     runST (runStateT runShuffle randGen)
>     where
>     runShuffle :: ShuffleState s (UArray Int Int)
>     runShuffle  = do a <- lift $ createArray bs
>                      doShuffle hi a
>                      lift $ unsafeFreeze a
>     doShuffle :: Int -> (STUArray s Int Int) -> ShuffleState s ()
>     doShuffle cur a
>         | cur> lo = 
>             do n <- getRandom $ boundsFilter bs cur
>                swapElems a cur n
>                doShuffle (cur - 1) a
>         | otherwise  = return ()
>     getRandom :: (Int, Int) -> ShuffleState s Int
>     getRandom bs = do r <- get
>                       (n, r') <- return $ randomR bs r
>                       put r'
>                       return n
>     swapElems :: (STUArray s Int Int) -> Int -> Int -> 
>                  ShuffleState s ()
>     swapElems a n1 n2 = do
>                         v1 <- lift $ readArray a n1
>                         v2 <- lift $ readArray a n2
>                         lift $ writeArray a n1 v2
>                         lift $ writeArray a n2 v1
>
>
    

The first thing we do is define our state type ShuffleState. Note that it is parameterized on both the type of the monadic value a and the ST monad type s. This is important. I originally tried only parameterizing on a and introducing s on the right side using forall. As with the non-State implementation the use of forall is the wrong thing to do. The compiler is smart enough to figure out what s should be in all the uses of ShuffleState.

The big changes in shuffleImpl' is that we put a call to runStateT inside the call to runST. This runs the computation in the combined ST and State monads. Our state is the random number generator. We no longer pass around the random number generator, instead we stick it in the state in the call to runStateT and then in getRandom we grab the generator from the state, get a random number and stick the updated generator back in the state. Otherwise things work mostly the same as in shuffleImpl modulo a few calls to lift that are needed to lift values from the ST monad into the combined monad. In our case we need to lift any value that is only in the ST monad, like the results of readArray and writeArray.

You might have noticed that shuffleImpl' is actually bigger than shuffleImpl. This is due to getRandom. While it is bigger, the actual code is a bit cleaner so I think it's worth the trade-off. If we were doing random number generation in more than just the one spot then we would probably see a net gain in code size.

So there you go, a quick tutorial on using mutable arrays in the ST array on it's own and with StateT.

   1. I suppose pedagogical could mean anal in this case. Normally I wouldn't declare the types of functions defined in a where clause but it seems instructive to do so in this case.↩

