module ProjectInfo.Graph
where

import ProjectInfo.Cproject
import Text.Regex.Posix((=~))
import Data.List(intersperse)

createDependencyGraph :: [ProjectDependency] -> String
createDependencyGraph xs = unlines $ "strict digraph FEM {": edges xs  ++ ["}"]
    where swcs = [x | (ProjectDependency x _) <- xs, x =~ "swc_*"] 

edges xs = map projectDep xs 
      where projectDep (ProjectDependency p deps) = foldl expressDep (simplify p ++ " ") (depsWithoutSwcs deps) ++ ";"
            expressDep result next = result ++ " -> " ++ next
            depsWithoutSwcs ds = filter (\x-> not $ x =~ "swc[a-zA-Z0-9_]*") ds
            simplify p = if p =~ "swc" then "swc" else p
