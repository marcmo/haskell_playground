import Char
f :: Ord a => a -> a -> Bool
f x y = x < y

neg :: Num a => (a -> a -> Bool) -> (a -> a -> Bool)
neg f = \x y -> not $ f x y

class Predicate a where
  complement :: a -> a

instance Predicate Bool where
  complement = not

instance (Predicate b) => Predicate (a -> b) where
  complement f = \a -> complement (f a)  
  -- if you want to be mysterious, then
  -- complement = (complement .)
  -- also works
  
v0 :: (Int, Char -> (String,Bool))
v0 = (3, \ c -> ([c,'q',c,'r'], isDigit c))

first  :: (a -> a') -> ((a,b) -> (a',b))
first  f = \ (a,b) -> (f a, b)

second :: (b -> b') -> ((a,b) -> (a,b'))
second g = \ (a,b) -> (a, g b)

result :: (b -> b') -> ((a -> b) -> (a -> b'))
result =  (.)
