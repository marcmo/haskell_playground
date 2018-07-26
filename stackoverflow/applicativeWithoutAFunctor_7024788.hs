import Control.Applicative 

data Image a = Image [a]
instance Applicative Image where
  pure xs = Image [xs]
  (Image a) <*> (Image b) = Image (a ++ b) 

map_ :: (Float -> Float) -> Image Float -> Image Float
map_ = undefined
zipWith_ :: (Float -> Float -> Float) -> Image Float -> Image Float -> Image Float
zipWith_ = undefined

-- foo ::  (Applicative f, Num b) => f b -> f b -> f b
-- foo :: Image Float -> Image Float -> Image Float
-- foo image1 image2 =
--   (++) <$> image1 <*> image2
-- (\x y z -> (x+y)/z) <$> i1 <*> i2 <*> i3 

