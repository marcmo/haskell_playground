import qualified Data.ByteString as B

toUpper x = if x >= chr 'a' && x <= chr 'z' then x - chr ' ' else x

fun a b | a == chr ' ' || b == chr '\n' = toUpper b
        | otherwise = b

convert = B.tail . B.scanl fun (chr ' ')

chr = fromIntegral . fromEnum

main = B.readFile "lorem.txt" >>= B.putStr . convert
