import Control.Applicative

  
foo :: Int -> Int
foo x = 2*x

newtype Maybe' a = Maybe' { getMaybe :: Maybe a}
instance Functor Maybe' where
  fmap _ (Maybe' Nothing) = Maybe' Nothing
  fmap g (Maybe' (Just x)) = Maybe' $ Just (g x)

instance Applicative Maybe' where
  pure x = Maybe' $ Just x
  Maybe' (Just f) <*> Maybe' (Just x) = Maybe' $ Just (f x)
  _ <*> _ = Maybe' Nothing

newtype Annotation e a = Annotation { getAn :: (,) e a } deriving (Show)

instance Functor (Annotation e) where
  fmap g (Annotation (e,x)) = Annotation (e,g x)

elem1 ::  (,) Char Integer
elem1 = ('a',1)

elem2 = Annotation ('a',1)

