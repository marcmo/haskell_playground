import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibIo :: Int -> IO ()
fibIo n = print $ fib n
force ::  Int -> Pure
force n = whnf fib n

main = defaultMain [
       bgroup "fib" [ bench "fib 8" $ force 8 
                    , bench "fib 15" $ force 15
                    ]
                   ]
