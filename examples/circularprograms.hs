module Circular
where
import List

fib:: [Integer]
fib = 1:1:[a+b | (a,b) <- zip fib (tail fib)]

fib2 = 1:1:(map (\(a,b) -> a+b) (zip fib2 (tail fib2)) )

stupidmap xs = 1:1:(map (\x -> x+1) xs)

meltList::Eq a => [a] -> [a] -> [a]
meltList xs ys = foldl (\sofar y -> if elem y ys then sofar else y:sofar) [] xs

meltList2 listToMelt refList = List.nubBy (\x y -> elem y refList) listToMelt
  
