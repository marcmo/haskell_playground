import System.Random

flipCoin :: StdGen -> Bool
flipCoin gen = fst $ random gen

data Cat = Cat String deriving Show
data Probable a = Dead | Live a deriving Show

felix = Live $ Cat "Felix"

flipCat :: StdGen -> a -> Probable a
flipCat gen cat = if (flipCoin gen) 
                  then Dead
                  else Live cat

data Schroedinger a
    = Opened (Probable a)
    | Unopened StdGen a deriving Show
    
  
felix2   = Opened (Live (Cat "Felix")) -- lucky Felix
poorGuy = Opened Dead                 -- DOA
unknown = Unopened (mkStdGen 100) (Cat "Felix")

instance Monad Schroedinger where
  Opened Dead >>= _ = Opened Dead
  Opened (Live a) >>= f = f a
  Unopened gen cat >>= f = Opened (flipCat gen cat) >>= f  
  return cat = Opened (Live cat)

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

rollDice :: IO Bool
rollDice = getStdRandom (randomR (True,False))

main = do
  gen <- getStdGen
  print (Unopened gen (Cat "Felix"))
  print (do
          box <- Unopened gen (Cat "Felix")
          -- The cat's fate is undecided
          return box)

