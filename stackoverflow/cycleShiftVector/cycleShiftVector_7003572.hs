import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (index, zipWith, Seq, (><), (<|), (|>))

seqSize = 100
numShifts = 10000 

cycleShift :: Integer -> Seq a -> Seq a
cycleShift s l = Seq.drop (fromInteger s) l >< Seq.take (fromInteger s) l

modAdd :: Seq Integer -> Seq Integer -> Seq Integer 
modAdd s t = Seq.zipWith (\ a b -> (a + b) `mod` 10^16) s t

step :: Seq Integer -> Integer -> Seq Integer
step l shift = modAdd l (cycleShift shift l)

allshifts = [i `mod` seqSize |i <- [1..numShifts]]
start = Seq.fromList (1 : [0 | i <- [1..(seqSize - 1)]])
end = foldl' step start allshifts

main :: IO ()
main = print (Seq.index end 0)

