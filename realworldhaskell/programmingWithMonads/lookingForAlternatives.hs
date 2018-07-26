
import Control.Monad

data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),(Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]

personalPhone :: [(Context, Phone)] -> Maybe Phone
personalPhone xs = case lookup Home xs of
                     Just x -> lookup Mobile xs
                     Nothing -> Nothing
                       
businessPhones = foldr (\(a,b) y -> 
                        case a of
                          Business -> b:y 
                          _        -> y) []

businessPhones2 xs = map snd findem
    where findem = case filter (category Business) xs of
                     [] -> filter (category Mobile) xs
                     ns -> ns
category c (a,_) = c == a

personalPhone3 xs = lookup Home xs `mplus` lookup Mobile xs
businessPhones3 xs = map snd rs
    where rs = filter (category Business) xs `mplus` 
               filter (category Mobile) xs
lookupM2 :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM2 _ [] = mzero
lookupM2 k ((a,b):xs)
    | a==k      = return b `mplus` lookupM2 k xs
    | otherwise = lookupM2 k xs
