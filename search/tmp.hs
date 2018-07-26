foo n = [0..n]
li = [(x,y) | x <- [1..4], y <- foo x]
li2 = [(x,y) | y <- foo x, x <- [1..4]]

