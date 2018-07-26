import System.Random -- for tests
import Control.Monad.State
import List(delete,(\\))

-- ** Naive -- lucid but inefficient -- implementation
-- 
-- First, a naive functional shuffle. We note that in the sequence
-- (r1,...rn), rn is always zero. Therefore, we pass to the function
-- 'shuffle' a sequence of n-1 random numbers.


extract:: Integer -> [a] -> (a,[a])
-- given a list l, extract the n-th element and return the element and
-- the remaining list. We won't worry about the order of the list
-- of the remaining elements. n>=0. n==0 corresponds to the first element.
extract 0 (h:t) = (h,t)
extract n l = loop n l []
	where
	    loop 0 (h:t) accum = (h,accum ++ t)
	    loop n (h:t) accum = loop (n-1) t (h:accum)

-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle:: [b] -> [Integer] -> [b]
shuffle [e] [] = [e]
shuffle elements (r:r_others) = let (b,rest) = extract r elements
				in b:(shuffle rest r_others)

-- Obviously the running time of "extract n l" is
-- O(length(l)). Therefore, the running time of shuffle is O(n^2).
-- 
-- 
-- ** Efficient implementation based on complete binary trees
-- 
-- The following is a more sophisticated algorithm:

-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a = Leaf a | Node !Int (Tree a) (Tree a) deriving Show

-- Convert a sequence (e1...en) to a complete binary tree
build_tree = grow_level . (map Leaf)
    where
    grow_level [node] = node
    grow_level l = grow_level $ inner l
	     
    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = (join e1 e2) : inner rest
	     
    join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
    join l@(Node ct _ _)  r@(Leaf _)       = Node (ct+1) l r
    join l@(Leaf _)       r@(Node ct _ _)  = Node (ct+1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl+ctr) l r

{-
-- example:
Main> build_tree ['a','b','c','d','e']
Node 5 (Node 4 (Node 2 (Leaf 'a') (Leaf 'b'))
               (Node 2 (Leaf 'c') (Leaf 'd')))
       (Leaf 'e')

-}

-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elements rseq = shuffle1' (build_tree elements) rseq
    where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (ri:r_others) = extract_tree ri tree 
				    (\tree -> shuffle1' tree r_others)
	     -- extract_tree n tree
	     -- extracts the n-th element from the tree and returns
	     -- that element, paired with a tree with the element
	     -- deleted (only instead of pairing, we use CPS)
	     -- The function maintains the invariant of the completeness
	     -- of the tree: all internal nodes are always full.
	     -- The collection of patterns below is deliberately not complete.
	     -- All the missing cases may not occur (and if they do,
	     -- that's an error.
    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf r)) k = r:k l
    extract_tree n (Node c l@Leaf{} r) k =
	extract_tree (n-1) r (\new_r -> k $ Node (c-1) l new_r)
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
				       
    extract_tree n (Node c l@(Node cl _ _) r) k
	| n < cl = extract_tree n l (\new_l -> k $ Node (c-1) new_l r)
	| otherwise = extract_tree (n-cl) r (\new_r -> k $ Node (c-1) l new_r)


-- examples

t1 = shuffle1 ['a','b','c','d','e'] [0,0,0,0]
-- "abcde"
-- Note, that rseq of all zeros leaves the sequence unperturbed.

t2 = shuffle1 ['a','b','c','d','e'] [4,3,2,1]
-- "edcba"
-- The rseq of (n-i | i<-[1..n-1]) reverses the original sequence of elements

t3 =  shuffle1 ['a','b','c','d','e'] [2,1,2,0]
-- "cbead"
-- Just some random shuffle.

-- The function build_tree builds a complete binary tree, of depth
-- ceil(log2(n)). The function 'extract_tree' traverses the tree and
-- rebuilds a truncated branch. This requires as many steps as the length
-- of the rebuilt branch, which is at most ceil(log2(n)). To be more
-- precise, the complexity of 'extract_tree' is ceil(log2(size(tree))),
-- because extract_tree keeps the tree complete. The function shuffle1'
-- invokes 'extract_tree' (n-1) times. Thus the overall complexity is
-- O(n*logn).
-- 
-- 
-- ** Performance test
-- 
-- To stress-test the implementation, we shuffle a sequence of one
-- million and one integers.

-- Given a source of randomness g and an integer n, produce
-- a list of [r1,...rn] of numbers such that ri is an independent sample
-- from a uniform random distribution [0..n-i+1]

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
  loop acc 0 g = (reverse acc,g)
  loop acc n g = let (r,g') = randomR (0,n) g 
		 in loop (r:acc) (pred n) g'
{-
*Shuffle> make_rs 3 (mkStdGen 800)
([3,2,1],419851139 2103410263)
*Shuffle> make_rs 3 (mkStdGen 80000)
([0,1,0],1034518374 2103410263)
-}

-- main = let n = 1000000 
       -- in print $ length $ shuffle1 [1..n+1] (fst $ make_rs n (mkStdGen 17))
main = let n = 1000
       in print $ shuffle1 [1..n+1] (fst $ make_rs n (mkStdGen 17))

-- We compile the code as
--    ghc -O2 sh1.hs # GHC 6.8.3, Intel(R) Pentium(R) 4 CPU 2.00GHz
-- and run it as
--    /usr/bin/time ./a.out +RTS -tstderr
-- producing
--   1000001
--   <<ghc: 1762688356 bytes, 3399 GCs, 
--          23738368/77729792 avg/max bytes residency (18 samples), 
--          217M in use, 
--          0.00 INIT (0.00 elapsed), 9.30 MUT (9.42 elapsed), 
--          14.39 GC (14.96 elapsed) :ghc>>
--        24.42 real        23.69 user         0.61 sys
-- 
-- Most of the running time (14 seconds) is spent collecting garbage;
-- the shuffling algorithm took only 9.30 seconds. There are many
-- opportunities for deforestation (e.g., using streams instead of
-- lists).

data CountedRandom = CountedRandom {
      crGen :: StdGen
    , crCount :: Int
    } deriving (Show)

type CRState = State CountedRandom
type RandomState a = State StdGen a
-- showCr :: CRState -> IO ()
-- showCr x = undefined
  -- print "hi"
getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val

getRandom :: Random a => RandomState a
getRandom = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val
  
getCountedRandom2 :: Random a => CRState [a]
getCountedRandom2 = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return [val]
{-- /snippet CountedRandom --}

myShuffle :: StdGen -> [Int] -> [Int]
myShuffle newStgen initial = inner initial [] newStgen

inner :: (RandomGen t) => [Int] -> [Int] -> t -> [Int]
inner [] accum gen = accum
inner xs accum gen = let (p,gen') = randomR(0,(length xs)-1) gen in
                      inner (delete (xs!!p) xs) ((xs!!p):accum) gen'

  {-- snippet getCount --}
getCount :: CRState Int
getCount = crCount `liftM` get
{-- /snippet getCount --}

{-- snippet putCount --}
putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }
{-- /snippet putCount --}

-- myshuffle :: [a] -> CRState Int -> [a]
myshuffle xs crs = do
  cr <- crs
  xs
  -- shuf [] xs (crGen cr)
  --   where shuf rs [] _ = rs
  --         shuf rs (y:ys) gen = shuf y:rs ys gen


