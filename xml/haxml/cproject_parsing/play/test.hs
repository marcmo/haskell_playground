{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Reader

data Test = Test {
  name :: String,
  age :: Int
}

foo :: Test -> String
foo (Test x _) = x

bar ::  (MonadReader r m, Num r) => t -> m r
bar x = do
  cfg <- ask
  return (2*cfg)

-- configUser :: (MonadReader Int m) => Int -> m [Char]
-- configUser = do
--   cnt <- ask :: Int
--   return ("this was it: " ++ show cnt)
myName ::  String -> Reader Int String
myName step = do
  myage <- ask -- :: String
  return (step ++ ", I am " ++ show myage)

myAge :: Reader Int Int
myAge = do
  age <- ask
  return $ age + 4

localExample :: Reader Int String
localExample = do
  a <- myName "First"
  age <- myAge
  multi <- multiStep
  return $ a ++ " " ++ show age ++ " " ++ show multi

multiStep :: Reader Int Int
multiStep = do
  second <- secondStep
  return $ second + 55

secondStep :: Reader Int Int
secondStep = do
  current <- ask
  return $ current*2
-- ex2 = do
--   a <- myName "oli"
--   return a
