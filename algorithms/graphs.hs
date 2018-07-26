module Main where
import Data.Graph.Inductive

main ::  IO ()
main = putStrLn clr595

-- kin248 ="digraph fgl {\\n\\tmargin = \\"0\\"\\n\\tpage = \\"8.5,11.0\\"\\n\\tsize = \\"11.0,8.5\\"\\n\\trotate = \\"90\\"\\n\\tratio = \\"fill\\"\\n\\t1 [label = \\"1\\"]\\n\\t2 [label = \\"2\\"]\\n\\t3 [label = \\"3\\"]\\n\\t4 [label = \\"4\\"]\\n\\t5 [label = \\"5\\"]\\n\\t6 [label = \\"6\\"]\\n\\t7 [label = \\"7\\"]\\n\\t8 [label = \\"8\\"]\\n\\t9 [label = \\"9\\"]\\n\\t10 [label = \\"10\\"]\\n\\t1 -> 7\\n\\t1 -> 4\\n\\t1 -> 2\\n\\t2 -> 5\\n\\t2 -> 4\\n\\t3 -> 10\\n\\t3 -> 4\\n\\t4 -> 8\\n\\t4 -> 5\\n\\t5 -> 3\\n\\t5 -> 2\\n\\t6 -> 7\\n\\t7 -> 8\\n\\t7 -> 6\\n\\t8 -> 10\\n\\t9 -> 10\\n\\t9 -> 9\\n\\t10 -> 9\\n\\t10 -> 8\\n}"

clr595 :: String
clr595 = "digraph fgl {\n\tmargin = \"0\"\n\tpage = \"8.5,11.0\"\n\tsize = \"11.0,8.5\"\n\trotate = \"90\"\n\tratio = \"fill\"\n\t1 [label = \"1\"]\n\t2 [label = \"2\"]\n\t3 [label = \"3\"]\n\t4 [label = \"4\"]\n\t5 [label = \"5\"]\n\t6 [label = \"6\"]\n\t1 -> 3 [label = \"13\"]\n\t1 -> 2 [label = \"16\"]\n\t2 -> 4 [label = \"12\"]\n\t2 -> 3 [label = \"10\"]\n\t3 -> 5 [label = \"14\"]\n\t3 -> 2 [label = \"4\"]\n\t4 -> 6 [label = \"20\"]\n\t4 -> 3 [label = \"9\"]\n\t5 -> 6 [label = \"4\"]\n\t5 -> 4 [label = \"7\"]\n}"

genLNodes :: Enum a => a -> Int -> [LNode a]
genLNodes q i = take i (zip [1..] [q..])

kin248           :: Gr Int ()
kin248 = mkGraph (genLNodes 1 10)
                 (labUEdges [(1,2),(1,4),(1,7),(2,4),(2,5),(3,4),(3,10),
                         (4,5),(4,8),(5,2),(5,3),(6,7),(7,6),(7,8),
                         (8,10),(9,9),(9,10),(10,8),(10,9)])
labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))




