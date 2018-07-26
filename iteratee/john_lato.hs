import Prelude hiding (drop,head,length)
import Control.Applicative

data StreamG el = Empty | El el | EOF deriving (Show)
data IterV el a = Done a (StreamG el)
                | Cont (StreamG el -> IterV el a)


enum :: IterV el a -> [el] -> IterV el a
enum i [] = i
enum i@(Done _ _) _ = i
enum (Cont k) (x:xs) = enum (k (El x)) xs

run :: IterV el a -> Maybe a
run (Done x _) = Just x
run (Cont k) = run' (k EOF)
  where run' (Done x _) = Just x
        run' _ = Nothing


head :: IterV el (Maybe el)
head = Cont step
  where step (El el) = Done (Just el) Empty
        step Empty = Cont step
        step EOF = Done Nothing EOF
-- Return the first element of the stream
peek :: IterV el (Maybe el)
peek = Cont step
  where step c@(El el) = Done (Just el) c
        step Empty = Cont step
        step EOF = Done Nothing EOF
-- Some iteratees are in the Done state before taking input
drop :: Int -> IterV el ()
drop 0 = Done () Empty
drop n = Cont step
  where step (El _) = drop (n-1)
        step Empty = Cont step
        step EOF = Done () EOF
-- Iteratees frequently need to keep an accumulator
length :: IterV el Int
length = Cont (step 0)
  where step acc (El _) = Cont (step (acc+1))
        step acc Empty = Cont (step acc)
        step acc EOF = Done acc EOF


instance Monad (IterV el) where
  return x = Done x Empty
  m >>= f = case m of
    Done x str -> case f x of
      Done x' _ -> Done x' str
      Cont k	-> k str
    Cont k -> Cont (\str -> k str >>= f)

instance Functor (IterV el) where
  fmap f (Done x str) = Done (f x) str
  fmap f (Cont k) = Cont (fmap f . k)

instance Applicative (IterV el) where
  pure x = Done x Empty
  (Done f str) <*> i2 = fmap f i2
  (Cont k) <*> i2 = Cont (\str -> k str <*> i2)

