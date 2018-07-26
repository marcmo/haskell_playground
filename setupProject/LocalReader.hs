{-- snippet localExample --}
module LocalReader where
  
import Control.Monad.Reader

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Int -> Reader String (String, String, String)
localExample x = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
{-- /snippet localExample --}
