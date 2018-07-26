{-# LANGUAGE DeriveDataTypeable #-}
 
import Data.Typeable
import Data.Data
import Data.Map
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
 
data Person = Person { name :: String
                     , age ::Int
                     } deriving (Data, Typeable)
 
-- joe =  Person { name = "Joe Bloggs", age = 23 }
names = ("Joe", "Bloggs")
joe = Data.Map.fromList [("name", "Joe Bloggs"), ("age", "23")]
 
t1 = newSTMP $ unlines [
  "Hello $names.0$",
  "Your full name is $person.name$, you are $person.age$ years old."
  ] :: StringTemplate String
 
main = putStrLn $ toString $ setAttribute "names" names $ setAttribute "person" joe t1

