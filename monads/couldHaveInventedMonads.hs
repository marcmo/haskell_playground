
f,g:: Float -> Float
f x = 3* x
g x = 2*x

f',g':: Float -> (Float,String)
f' x = (3*x,"f was called")
g' x = (2*x,"g was called")

fg :: Float -> Float -> (Float,String)
fg x y = let (y,s) = g' x 
             (z,t) = f' y in (z,s++t)

bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f (x,s) = let (fx,fs) = f x in (fx,s++fs)
data Comp a = Com a
bind3 :: (a -> Comp a) -> (Comp a -> Comp a)
bind3 f x = let Comp fx = f x in x

unit x = (x,"")

lift f = unit.f

foo = bind f' (g' 3)
foo2 = bind f' (22,"hi")
foo3 = bind f'.g'
foo4 = bind (lift f).g'
foo5 = bind f'
foo6 = bind g'

cbrt x = x**(1/3)
sixthroot = cbrt.sqrt

cbrt' x = [x**(1/3),-(x**(1/3))]
sqrt' x = [x**(1/2),-(x**(1/2))]
bar x = [x,2*x,3*x]
bind2 f x = concat (map f x)
ident x = [x]

foo7 = bind2 cbrt'
