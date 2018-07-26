{-- snippet module --}
module Main () where

import Control.Monad.Reader

import SimpleJSON
import LocalReader

main = do
  print (JObject [("foo", JNumber 1), ("bar", JBool False)])
  let r = runReader (localExample 3) "Fred"
  print $ show r
{-- /snippet module --}
