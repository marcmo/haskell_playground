module Monadtesting
    where
import Monad

foo :: Int -> Maybe String
foo 1 = Just "one"
foo _ = Nothing

lifi :: (String -> Int) -> (Maybe String -> Maybe Int)
lifi f m = m >>= \s -> return (f s)
