import qualified Data.ByteString.Lazy.Char8 as L

test = L.pack "1,2,3,4,5"
closing = readPrice . (!!4) . L.split ','

readPrice s =
  case L.readInt s of
    Nothing             -> Nothing
    Just(dollars,rest)  ->
      case L.readInt (L.tail rest) of
        Nothing         -> Nothing
        Just (cents,_)  ->
          Just (dollars * 100 + cents)