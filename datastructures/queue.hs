
data Queue a = Q ([a],[a])
instance Show a => Show (Queue a) where
  show (Q (f,r)) = show f ++ show (reverse r)

snocQ :: Queue a -> a -> Queue a
snocQ (Q ([],_)) x = Q ([x],[]) 
snocQ (Q (f,r)) x = Q (f,x:r)

tailQ :: Queue a -> Queue a
tailQ (Q ([x],r)) = Q (reverse r,[])
tailQ (Q (x:xs,r)) = Q (xs,r)

headQ :: Queue a -> a
headQ (Q ((x:_),_)) = x

emptyQ ::  Queue a
emptyQ = Q ([],[])

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q (xs,_)) = null xs



