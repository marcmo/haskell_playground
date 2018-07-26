{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign
import Foreign.C.Types

foreign import ccall "test.h foo"
     c_foo :: CInt -> CInt
-- int foo(int);
-- int getBuffer(uint8_t** buffer);
foreign import ccall unsafe "test.h getBuffer"
     c_getBuffer :: Ptr Word8 -> CInt

fastfoo x =  c_foo (fromInteger x)

main = do
    mapM_ (print . fastfoo) [1..10]
    
