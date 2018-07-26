
class CollE s where
    empty  :: s

class CollE s => Coll s a where
    insert :: s -> a -> s

instance Coll [a] a where
    insert xs x = x:xs
instance CollE [a] where
    empty = []
