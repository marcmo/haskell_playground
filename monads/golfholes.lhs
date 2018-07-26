I hope you're not tired of receiving other people's implementations on here... I am learning myself and thought a reimplementation of your program here would be fun. And it was! So I hope my version elucidates some things, with a little commentary.

It took a long time for me to get comfortable enough with monads that I could do IO the way I do in this program. I laud your implementation and I hope you'll keep on keepin' on. Haskell is hard to learn and the type system often doesn't help. I started including annotations because everyone else does and it actually seems to help find bugs and improve the design.

> module GolfScore where
> import Data.List -- for the sort function

One thing a friend told me is that you are going to want functions that take a simple type and return stuff inside a monad, rather than monkeying around with monad -> monad typed functions. Actually in general do it as purely as possible; if you have to return in a monad that's fine but if you can return a pure value instead, do that; there are ways of lifting that function into the monad when you have to. Because the monad operations are all intended to help you glue functions with that kind of type together. Which is what I've got in this code:

> getName :: IO String
> getName = putStr "Enter name: " >> getLine
>
> getScore :: IO Integer
> getScore = putStr "Enter score: " >> getLine >>= return . read

Then I combined these primitives to make one which handles getting a person's name and score. I decided it would be more convenient to carry them around in a 2-tuple rather than two lists, because it makes the relationship concrete.

> getNameAndScore :: Int -> IO (String, Integer)
> getNameAndScore holes = do
>                         name <- getName
>                         scores <- sequence $ replicate holes getScore
>                         return (name, sum scores)

Sequence and replicate form the basis of my iteration in this program. It lets me avoid any explicit looping, because we're really just doing the same things over again and collecting the result. I recommend spending time reading the prelude, it helped me quite a bit to see what's there and built-in already. Anyhow, sequence and sequence_ are handy because they let you take a list of monadic actions and convert them into a list of results. mapM is similar, essentially applying a monad action to a list and collecting the result.

Here are my output functions:

> showResult :: String -> Integer -> IO ()
> showResult name score = putStrLn $ name ++ " got a score of " ++ (show score)
>
> showResults :: [(String, Integer)] -> IO ()
> showResults = mapM_ (uncurry showResult)

So showResults is clearly intended to take the output of getNameAndScore. showResult probably could have been coded to take a 2-tuple but I like using uncurry and I don't like writing functions that take tuples unless I really have to. Pet peeve I guess. And there's the aforementioned mapM. The underscore variants tend to throw away their results, which is fine here, because showResult produces IO (), and mapM would give me back a list of [IO (), IO ()...]. Not something I care to keep around.

And here is your essential function. I did add one thing, it sorts by score, showing the winner at the top. :)

> run :: Int -> Int -> IO ()
> run count holes = do
>                   namesAndScores <- sequence $ replicate count (getNameAndScore holes)
>                   let sorted = sortBy (\l r -> compare (snd l) (snd r)) namesAndScores
>                   showResults sorted

The sequence $ replicate construct there is the same as the one above, creating a list of monad actions and then evaluating them. I had a tendency when I was starting to try and write the second line as "sorted <- sortBy ..." but that of course doesn't work because it is a pure function result. The let solves this problem. sortBy takes a function and a list; my function just pushes the compare into the second item of the tuple and sorts based on that, which happens to be the score. Then I show the results.
