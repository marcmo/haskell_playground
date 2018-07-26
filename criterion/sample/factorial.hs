import Criterion.Main

main = defaultMain [bgroup "fac" [bench (show n) (nf product [1..n]) | n <- ns]]
        where ns = [k*10^4 | k <- [1,2,3]] :: [Integer]
