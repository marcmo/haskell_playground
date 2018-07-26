data Tree = Empty | CreateTree Integer Tree Tree

flattenInfix Empty = []
flattenInfix (CreateTree v l r) = flattenInfix(l) ++ [v] ++ flattenInfix(r)

flattenPrefix Empty = []
flattenPrefix (CreateTree v l r) = [v] ++ flattenPrefix(l) ++ flattenPrefix(r)

flattenPostfix Empty = []
flattenPostfix (CreateTree v l r) = flattenPostfix(l) ++ flattenPostfix(r) ++ [v]

insert x Empty = (CreateTree x Empty Empty)
insert x (CreateTree v l r) =
    if x<v then CreateTree v (insert x l) r
    else CreateTree v l (insert x r)

putLeft Empty Empty = Empty
putLeft Empty a = a
putLeft a Empty = a
putLeft a (CreateTree e l r) = (CreateTree e (putLeft a l) r)

remove x Empty = Empty
remove x (CreateTree e l r) =
    if x==e then putLeft l r
    else if x<e then (CreateTree e (remove x l) r)
         else (CreateTree e l (remove x r))

tree = (CreateTree 20
        (CreateTree 10
         (CreateTree 5
          (CreateTree 1 Empty Empty)
          (CreateTree 7 Empty Empty))
         (CreateTree 15 Empty Empty))
        (CreateTree 30
         (CreateTree 25 Empty Empty)
         (CreateTree 35 Empty Empty)))
