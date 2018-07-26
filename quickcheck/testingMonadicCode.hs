import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import Data.STRef
import Control.Monad.ST

data Element s a = Element a (STRef s (Link s a))
data Link s a = Weight Int | Next (Element s a)
newElement :: a -> ST s (Element s a)
newElement a = do
    r <- newSTRef (Weight 1)
    return (Element a r)

findElement :: Element s a -> ST s (Element s a)
findElement (Element a r) = do
    e <- readSTRef r
    case e of
      Weight _ -> return (Element a r)
      Next next -> do
          _last <- findElement next
          writeSTRef r (Next _last)
          return _last

unionElements :: Element s a -> Element s a -> ST s ()
unionElements e1 e2 = do
    Element a1 r1 <- findElement e1
    Element a2 r2 <- findElement e2
    Weight w1 <- readSTRef r1
    Weight w2 <- readSTRef r2
    if w1 <= w2
      then do
        writeSTRef r1 (Next (Element a2 r2))
        writeSTRef r2 (Weight (w1 + w2))
      else do
        writeSTRef r2 (Next (Element a1 r1))
        writeSTRef r1 (Weight (w1 + w2))

instance Eq (Element s a)
  where Element _ r == Element _ r' = r == r'

data Action = New | Find Int | Union Int Int
  deriving (Show)

actions :: Int -> Gen [Action]
actions 0 =
  frequency [(25,liftM (New :) (actions 1)),
             (1, return [])]
actions n =
  frequency
    [(2,liftM (New :) (actions (n + 1))),
     (2,liftM2 (:) (liftM Find element)
     (actions n)),
     (2,liftM2 (:) (liftM2 Union element
                                   element)
                    (actions n)),
     (1,return [])]
  where
    element = choose (0,n-1)

pickElement :: Monad m => [a] -> PropertyM m a
pickElement vars = do
  pre (not (null vars))
  i <- pick (choose (0, length vars - 1))
  return (vars !! i)

testit :: Int -> IO Int
testit x = do
  putStrLn $ "tryint testit with " ++ show x
  return x

prop_testit :: Int -> Property
prop_testit x = monadicIO $ do
      res <- run $ testit x
      assert $ res == 1

