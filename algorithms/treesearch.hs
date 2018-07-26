

   -- 1 ----20---- 2 ----15------ 4
   --    \                      /
   --     \     +------20------+
   --      10  /  
   --       \ /
   --        3

import Debug.Trace

type Node = Int
type Vertice = (Node,Node)

treesearch :: ([Node], Node, Node -> [Node], [Node] -> (Node, [Node])) -> [Node]
treesearch (initial,goal,children,removeChoice) = loopD initial []
    where loop [] _ = (trace "nothing")[]
          loop frontier path = let (s,rest) = removeChoice frontier in
                      if s == goal then reverse (s:path)
                      	else loopD ((children s) ++ rest) (s:path)
          loopD f p = (trace $ "loop " ++ show f ++ " " ++ show p) loop f p

childs 1 = [2,3]
childs 2 = [4]
childs 3 = [4]
childs 4 = []
pick xs = (trace $ "pick " ++ show (head rs))(head rs,tail rs)
  where rs = reverse xs
test = treesearch ([1],4,childs,pick)
          

