module Main where

-- http://www.haskell.org/haskellwiki/Algebraic_data_type
-- Rose Tree
data Tree a = Node a [Tree a]

df :: Tree a -> [a]
df (Node n st) = n : concat (map df st)
-- df (Node n st) = n : concat [df t | t <- st]

depthFirst :: Tree a -> [a]
depthFirst t = depthFirst' [t]
  where
  depthFirst' []                       = []
  depthFirst' ((Node n subTrees) : ts) = n : depthFirst' (subTrees ++ ts)

breadthFirst :: Tree a -> [a]
breadthFirst t = breadthFirst' [t]
  where
  breadthFirst' []                       = []
  breadthFirst' ((Node n subTrees) : ts) = n : breadthFirst' (ts ++ subTrees)

travel :: Tree a -> Bool -> [a]
travel t depthFirst = travel' [t]
  where 
  travel' [] = []
  travel' ((Node n st) : ts) = n : travel' nexts
    where nexts = if depthFirst then st ++ ts else ts ++ st

tree :: Tree Int
tree =   -- http://en.wikipedia.org/wiki/File:Depth-first-tree.svg
  Node 1 [
    Node 2 [
      Node 3 [
        Node 4 [], 
        Node 5 []
      ],
      Node 6 []
    ],
    Node 7 [],
    Node 8 [
      Node 9 [
        Node 10 [],
        Node 11 []
      ],
      Node 12 []
    ]
  ]

main :: IO ()
main = do 
       putStrLn $ show $ df tree
       putStrLn $ show $ depthFirst tree
       putStrLn $ show $ breadthFirst tree
       putStrLn $ show $ travel tree True
       putStrLn $ show $ travel tree False


