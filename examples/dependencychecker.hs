
data Node a = Node a

class Dep a where
    getDeps :: a -> [a]

-- instance Dep (Comp x y) where
--     getDeps (Comp x y) = y

--calcDeps :: Dep a => [a] -> [a] -> a -> [a]
--calcDeps processed deps x = 
--     if elem x processed then []
--     else 
--        let ds = (getDeps x)-- ++ deps 
--             fun = calcDeps (x:processed) ds
--         in [calcDeps processed ds d | d <- ds]
-- test [] x = [x]
-- test xs x = [test xs y | y <- getDeps x]
