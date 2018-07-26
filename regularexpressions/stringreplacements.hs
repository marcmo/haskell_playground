-- {-# LANGUAGE OverloadedStrings #-}

Looping in haskell can almost always be realized using a fold.  

So in your example you need to build your result by consecutively replacing strings taken from your mapping.  

Let's use a strict fold:

    import Data.List(foldl')

Then your `strRep` would look like:

    strRep :: M -> String -> String
    strRep m input = foldl' replace input m

Or a little shorter:

    strRep2 = flip $ foldl' replace

Dealing with `String`s is performing rather poorly. A better alternative is to deal with `Text` from `Data.Text`.  
Then `replace` is rather straight forward:

    import qualified Data.Text as T

    replace :: String -> (String,String) -> String
    replace s (a,b) = let [ss,aa,bb] = [T.pack x | x <- [s,a,b]] in
      T.unpack $ T.replace aa bb ss


