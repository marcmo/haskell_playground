import Criterion.Main
import Criterion.Config
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Codec.Crypto.RSA
import Crypto.Random.DRBG

main :: IO ()
main = do
  r1 <- newGenIO :: IO HashDRBG
  r2 <- newGenIO :: IO (GenBuffered HashDRBG)

  -- We don't care about the performance of generate, so we do it outside the benchmark framework
  let (pub,priv,g2) = generateKeyPair r2 1024

  defaultMainWith defaultConfig (return ()) [
    bgroup "RSA" [
       bench "1" $ whnf (enc r1 pub priv) m1
       , bench "2" $ whnf (enc r2 pub priv) m1
     ]
   ]

m1 :: L.ByteString
m1 = L.pack [0..63]

enc :: CryptoRandomGen g => g -> PublicKey -> PrivateKey -> L.ByteString -> L.ByteString
enc g pub priv m = 
    let (em,ng) = encrypt g pub m
        dm     = decrypt   priv em 
    in dm
