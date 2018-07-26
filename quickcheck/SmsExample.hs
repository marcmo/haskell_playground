module SmsExample where

import Data.List
import Data.Bits
import Data.Char
import Word
import Test.QuickCheck --hiding ( (.&.) )

-- An implementation of character packing and unpacking,
-- following very closely the specification in
-- GSM TS 03.38 v5.2.0 subclause 6.1.2.1.1.

--------------------------------- The code -----------------------------
pack :: [Word8] -> [Word8]
pack []			  = []
pack [a]		  = [a `shiftL` 1]
pack [a,b]   	     	  = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 ]			-- 6 bits b
pack [a,b,c] 	     	  = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 .|. c `shiftR` 5,	-- 6 bits b, 2 bit c
	     	     	     c `shiftL` 3 ] 			-- 5 bits c, 3 bit d
pack [a,b,c,d]       	  = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 .|. c `shiftR` 5,	-- 6 bits b, 2 bit c
	     	     	     c `shiftL` 3 .|. d `shiftR` 4,	-- 5 bits c, 3 bit d
			     d `shiftL` 4 ]			-- 4 bits d
pack [a,b,c,d,e]     	  = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 .|. c `shiftR` 5,	-- 6 bits b, 2 bit c
	     	     	     c `shiftL` 3 .|. d `shiftR` 4,	-- 5 bits c, 3 bit d
			     d `shiftL` 4 .|. e `shiftR` 3,	-- 4 bits d, 4 bit e
			     e `shiftL` 5 ] 			-- 3 bits e
pack [a,b,c,d,e,f]   	  = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 .|. c `shiftR` 5,	-- 6 bits b, 2 bit c
	     	     	     c `shiftL` 3 .|. d `shiftR` 4,	-- 5 bits c, 3 bit d
			     d `shiftL` 4 .|. e `shiftR` 3,	-- 4 bits d, 4 bit e
			     e `shiftL` 5 .|. f `shiftR` 2,	-- 3 bits e, 5 bit f
		  	     f `shiftL` 6 ]			-- 2 bits f
pack [a,b,c,d,e,f,g] 	  = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 .|. c `shiftR` 5,	-- 6 bits b, 2 bit c
	     	     	     c `shiftL` 3 .|. d `shiftR` 4,	-- 5 bits c, 3 bit d
			     d `shiftL` 4 .|. e `shiftR` 3,	-- 4 bits d, 4 bit e
			     e `shiftL` 5 .|. f `shiftR` 2,	-- 3 bits e, 5 bit f
		  	     f `shiftL` 6 .|. g `shiftR` 1,	-- 2 bits f, 6 bit g
			     g `shiftL` 7 ]			-- 1 bits g
pack (a:b:c:d:e:f:g:h:rs) = [a `shiftL` 1 .|. b `shiftR` 6,	-- 7 bits a, 1 bit b
	     	     	     b `shiftL` 2 .|. c `shiftR` 5,	-- 6 bits b, 2 bit c
	     	     	     c `shiftL` 3 .|. d `shiftR` 4,	-- 5 bits c, 3 bit d
			     d `shiftL` 4 .|. e `shiftR` 3,	-- 4 bits d, 4 bit e
			     e `shiftL` 5 .|. f `shiftR` 2,	-- 3 bits e, 5 bit f
		  	     f `shiftL` 6 .|. g `shiftR` 1,	-- 2 bits f, 6 bit g
			     g `shiftL` 7 .|. h ]		-- 1 bits g, 7 bit h
			     ++ pack rs


unpack :: [Word8] -> [Word8]
unpack [] = []
unpack [a]		  = [mask7 (a `shiftR` 1)]
unpack [a,b] 		  = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6)]
unpack [a,b,c] 		  = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6),
			     mask7 (c `shiftR` 3 .|. b `shiftL` 5)]
unpack [a,b,c,d]	  = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6),
			     mask7 (c `shiftR` 3 .|. b `shiftL` 5),
			     mask7 (d `shiftR` 4 .|. c `shiftL` 4)]
unpack [a,b,c,d,e]	  = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6),
			     mask7 (c `shiftR` 3 .|. b `shiftL` 5),
			     mask7 (d `shiftR` 4 .|. c `shiftL` 4),
			     mask7 (e `shiftR` 5 .|. d `shiftL` 3)]
unpack [a,b,c,d,e,f]	  = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6),
			     mask7 (c `shiftR` 3 .|. b `shiftL` 5),
			     mask7 (d `shiftR` 4 .|. c `shiftL` 4),
			     mask7 (e `shiftR` 5 .|. d `shiftL` 3),
			     mask7 (f `shiftR` 6 .|. e `shiftL` 2)]
unpack [a,b,c,d,e,f,g]    	-- The guard tests that the left-over 7 bits (which
				-- could in principle be another char, are zero)
  | mask7 g == 0 	  = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6),
			     mask7 (c `shiftR` 3 .|. b `shiftL` 5),
			     mask7 (d `shiftR` 4 .|. c `shiftL` 4),
			     mask7 (e `shiftR` 5 .|. d `shiftL` 3),
			     mask7 (f `shiftR` 6 .|. e `shiftL` 2),
			     mask7 (g `shiftR` 7 .|. f `shiftL` 1)]
unpack (a:b:c:d:e:f:g:rs) = [mask7 (a `shiftR` 1),
			     mask7 (b `shiftR` 2 .|. a `shiftL` 6),
			     mask7 (c `shiftR` 3 .|. b `shiftL` 5),
			     mask7 (d `shiftR` 4 .|. c `shiftL` 4),
			     mask7 (e `shiftR` 5 .|. d `shiftL` 3),
			     mask7 (f `shiftR` 6 .|. e `shiftL` 2),
			     mask7 (g `shiftR` 7 .|. f `shiftL` 1),
			     mask7 g]
			     ++ unpack rs

mask7 :: Word8 -> Word8
mask7 w = w .&. fromIntegral 127


----------------------------  Manual Testing -----------------------
testString :: [Char] -> Bool
testString s = toString (unpack (pack (fromString s))) == s

fromString :: [Char] -> [Word8]
fromString = map (fromIntegral . ord)
 
toString :: [Word8] -> [Char]
toString = map (chr . fromIntegral)

manualTests :: Bool
manualTests 
  = all testString [
		"",
		"1", 
		"12",
		"123",
		"1234",
		"12345",
		"123456",
		"1234567",
		"12345678",
		"123456789",
		"1234567890"]
		

---------------------------- Automated Testing -----------------------
-- QuickCheck testing
prop_pack :: [Word8] -> Bool
prop_pack s = unpack (pack s') == s'
  where
   s' = map mask7 s

-- test2 :: Property
prop_pack2 = forAll (vectorOf 8 arbitrary) prop_pack

instance Arbitrary Word8 where
   arbitrary = do { i <- arbitrary :: Gen Int
		  ; return (fromIntegral (abs i)) }


quickCheckN n = quickCheckWith n (3*n) 300



----------------------------------
-- Useful for an illustration in the talk

prop_ins :: Int -> [Int] -> Property
prop_ins x xs = ordered xs ==> collect (length xs) $
			       ordered (insert x xs)

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

