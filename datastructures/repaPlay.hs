{-# LANGUAGE TypeOperators #-}
import Data.Array.Repa

main = do
	-- let x = fromList (Z :. (10::Int)) [1..10]
	putStrLn $ show 3

-- foo = fromList (Z :. (10::Int)) [1..10]
foo = fromList (Z :. 3 :. 3) [1..9] :: Array ((Z :. Int) :. Int) Int

