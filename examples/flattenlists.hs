

data ListOfLists a = Uni a | Nested [ListOfLists a]

flat :: ListOfLists a -> [a]
flat (Uni xs) = [xs]
flat (Nested ys) = concat [flat y | y <- ys]

xs = Nested [Uni 2,Uni 3,Uni 4]
ys = Nested [Nested [Uni 2,Uni 3,Uni 4], Nested [Uni 3,Uni 4], Nested [Nested [Uni 2,Uni 3]]]


test = flat xs
test2 = flat ys

