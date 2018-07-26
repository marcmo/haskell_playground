module StackImpl
where

class StackImpl s a where
    pushImpl :: a->s->s
    popAndtopImpl :: s->Maybe (s,a)

instance StackImpl [a] a where
    pushImpl = (:)
    popAndtopImpl [] = Nothing
    popAndtopImpl (x:xs) = Just (xs, x)

data Stack a = forall s. StackImpl s a => Stck s

push :: a -> Stack a -> Stack a
push x (Stck s) = Stck (pushImpl x s)

pop :: Stack a -> Stack a
pop (Stck s) = case (popAndtopImpl s) of
                 Just (s',x::a) -> Stck s' -- (1)
top :: Stack a -> a
top (Stck s) = case (popAndtopImpl s) of
                 Just (_,x) -> x
empty :: Stack a -> Bool
empty (Stck s) = case (popAndtopImpl s) of
                   Just (s'::s,x::a) -> False -- (2)
                   Nothing -> True
