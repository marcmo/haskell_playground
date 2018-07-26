{-# LANGUAGE TemplateHaskell #-}
-- Haskell as an ultimate "smoke testing" tool
-- 
-- OR
-- 
-- Using QuickCheck as a DIY test data generator
-- Contents
-- [hide]
-- 
--     * 1 Preface
--     * 2 The fame of Quick Check
--     * 3 Concept of the Variant
--     * 4 Helper tools
--     * 5 Producing test data
--     * 6 Deriving Variant instances automagically
-- 
-- [edit]
-- 1 Preface
-- 
-- Recently, my wife approached me with the following problem: they had to test their re-implementation (in Java) of the part of the huge software system previously written in C++. The original system is poorly documented and only a small part of the sources were available.
-- 
-- Among other things, they had to wrote a parser for home-brewn DSL designed to describe data structures. The DSL is a mix of ASN.1 and BNF grammars, describes a structure of some data records and simple business rules relevant to processing of said record. The DSL is not Turing-complete, but allows user to define it's own functions, specify math and boolean expression on fields and was designed as "ASN.1 on steroids".
-- 
-- Problem is, that their implementation (in JavaCC) on this DSL parser was based on the single available description of the DSL grammar, which was presumably incomplete. They tested implementation on several examples available, but the question remained how to test the parser on a large subset of data in order to be fairly sure that "everything works"
-- [edit]
-- 2 The fame of Quick Check
-- 
-- My wife observed me during the last (2005) ICFP contest and was amazed at the ease with which our team has tested our protocol parser and printer using Quick Check. So, she asked me whether it is possible to generate pseudo-random test data in the similar manner for use "outside" of Haskell?
-- 
-- "Why not?" I thought. After all, I found it quite easy to generate instances of 'Arbitrary' for quite complex data structures.
-- [edit]
-- 3 Concept of the Variant
-- 
-- The task was formulated as follows:
-- 
--     * The task is to generate test datasets for the external program. Each dataset consists of several files, each containing 1 "record" 
-- 
--     * A "record" is essentially a Haskell data type 
-- 
--     * We must be able to generate pseudo-random "valid" and "invalid" data, to test that external program consumes all "valid" samples and fails to consume all "invalid" ones. Deviation from this behavior signifies an error in external program. 
-- 
-- Lets capture this notion of "valid" and "invalid" data in a type class:
-- 
module Variant where
 
import Control.Monad
import Test.QuickCheck
import System.Random
import System.IO
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Syntax hiding (Name)
 
class Variant a where
  valid   :: Gen a
  invalid :: Gen a

-- So, in order to make a set of test data of some type, the user must provide means to generate "valid" and "invalid" data of this type.
-- 
-- If we can make a "valid" Foo (for suitable "data Foo = ...") and "invalid" Foo, then we should also be able to make a "random" Foo:

instance Variant a => Arbitrary a where
  arbitrary   = oneof [valid, invalid]

-- Thus, taking for example the following definition for our "data-to-test":

data Record     = InputRecord Name Number
               | OutputRecord Name Number OutputType deriving Show
data Number     = Number String                       deriving Show
data Name       = Name String                         deriving Show
data OutputType = OutputType String                   deriving Show

-- we could produce the following instances of the class "Variant":

-- For definition of `neStringOf` see below, for now it is sufficient
-- to say that `neStringOf first next` produces non-empty string whose
-- first character is taken from `first` and all subsequent - from
-- `next`
garbledString = neStringOf ".,_+-" "abc0!@#$%^&*()."
instance Variant Number where
  valid   = liftM Number $ resize 4 $ neStringOf "123456789" "0123456789"
  invalid = liftM Number $ resize 4 $ garbledString
instance Variant Name where
  valid   = liftM Name $ elements [ "foo", "bar", "baz" ]
  invalid = liftM Name garbledString
instance Variant OutputType where
  valid   = liftM OutputType $ elements [ "Binary", "Ascii" ]
  invalid = liftM OutputType garbledString
 
instance Variant Record where
  valid   = oneof [ liftM2 InputRecord  valid   valid
                  , liftM3 OutputRecord valid   valid   valid ]
  invalid = oneof [ liftM2 InputRecord  valid   invalid
                  , liftM2 InputRecord  invalid valid
                  , liftM2 InputRecord  invalid invalid
                  , liftM3 OutputRecord invalid valid   valid 
                  , liftM3 OutputRecord valid   invalid valid 
                  , liftM3 OutputRecord valid   valid   invalid
                  , liftM3 OutputRecord invalid invalid valid 
                  , liftM3 OutputRecord valid   invalid invalid 
                  , liftM3 OutputRecord invalid valid   invalid
                  , liftM3 OutputRecord invalid invalid invalid
                  ]

-- The careful reader will have already spotted that once we hand-coded the instances of 'Variant' for a few "basic" types (like 'Name', 'Number', 'OutputType' etc), defining instances of Variant for more complex datatypes becomes easy, though quite a tedious job. We call to the rescue a set of simple helpers to facilitate this task
-- [edit]
-- 4 Helper tools
-- 
-- It could easily be seen that we consider an instance of a data type to be "invalid" if at least one of the arguments to the constructor is "invalid", whereas a "valid" instance should have all arguments to data type constructor to be "valid". This calls for some permutations:

proper1 f = liftM  f valid
proper2 f = liftM2 f valid valid
proper3 f = liftM3 f valid valid valid
proper4 f = liftM4 f valid valid valid valid
proper5 f = liftM5 f valid valid valid valid valid
 
bad1 f = liftM f invalid
bad2 f = oneof $ tail [ liftM2 f g1 g2 | g1<-[valid, invalid], g2<-[valid, invalid] ]
bad3 f = oneof $ tail [ liftM3 f g1 g2 g3 | g1<-[valid, invalid], g2<-[valid, invalid], g3<-[valid, invalid] ]
bad4 f = oneof $ tail [ liftM4 f g1 g2 g3 g4 | g1<-[valid, invalid], g2<-[valid, invalid], g3<-[valid, invalid], g4<-[valid, invalid] ]
bad5 f = oneof $ tail [ liftM5 f g1 g2 g3 g4 g5 | g1<-[valid, invalid], g2<-[valid, invalid], g3<-[valid, invalid], g4<-[valid, invalid], g5<-[valid, invalid] ]

-- With those helper definitions we could rewrite our Record instance as follows:

instance Variant Record where
  valid   = oneof [ proper2 InputRecord
                  , proper3 OutputRecord ]
  invalid = oneof [ bad2 InputRecord
                  , bad3 OutputRecord ]

-- Note the drastic decrease in the size of the declaration!

-- Oh, almost forgot to include the code for "neStringOf":

neStringOf chars_start chars_rest =
  do s <- elements chars_start
     r <- listOf' $ elements chars_rest
     return (s:r)
 
listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf' k gen
 
vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]

-- [edit]
-- 5 Producing test data
-- 
-- OK, but how to use all those fancy declarations to actually produce some test data?
-- 
-- Let's take a look at the following code:
-- 
data DataDefinition = DataDefinition Name Record
 
-- main = 
--   do let num = 200       -- Number of test cases in each dataset.
--      let config =        -- Describe several test datasets for "DataDefinition"
--                          -- by defining how we want each component of DataDefinition
--                          -- for each particular dataset - valid, invalid or random
--            [ ("All_Valid",       "txt",  num, (valid,     valid    ))
--            , ("Invalid_Name",    "txt",  num, (invalid,   valid    ))
--            , ("Invalid_Record" , "txt" , num, (valid,     invalid  ))
--            , ("Random",          "txt",  num, (arbitrary, arbitrary))
--            ]
     -- mapM_ create_test_set config
 
-- create_test_set (fname, ext, count, gens) =
--   do rnd <- newStdGen 
--      let test_set = generate 100 rnd $ vectorOf' count (mkDataDef gens)
--      sequence_ $ zipWith (writeToFile fname ext) [1..] test_set 
--   where
--   mkDataDef (gen_name, gen_rec) = liftM2 DataDefinition gen_name gen_rec
 
writeToFile name_prefix suffix n x =
  do h <- openFile (name_prefix ++ "_" ++ pad n ++ "." ++ suffix) WriteMode 
     hPutStrLn h $ show x
     hClose h 
  where pad n = reverse $ take 4 $ (reverse $ show n) ++ (repeat '0')

-- You see that we could control size, nature and destination of each test dataset. This approach was taken to produce test datasets for the task I described earlier. The final Haskell module had definitions for 40 Haskell datatypes, and the topmost datatype had a single constructor with 9 fields.
-- 
-- This proved to be A Whole Lot Of Code(tm), and declaration of "instance Variant ..." proved to be a good 30% of total amount. Since most of them were variations of the "oneof [proper Foo, proper2 Bar, proper4 Baz]" theme, I started looking for a way so simplify/automate generation of such instances.
-- [edit]
-- 6 Deriving Variant instances automagically
-- 
-- I took a a post made by Bulat Ziganshin on TemplateHaskell mailing list to show how to derive instances of 'Show' automatically, and hacked it to be able to derive instances of "Variant" in much the same way:

 
data T3 = T3 String
 
deriveVariant t = do
  -- Get list of constructors for type t
  TyConI (DataD _ _ _ constructors _)  <-  reify t
 
  -- Make `valid` or `invalid` clause for one constructor:
  --   for "(A x1 x2)" makes "Variant.proper2 A"
  let mkClause f (NormalC name fields) = 
        appE (varE (mkName ("Variant."++f++show(length fields)))) (conE name)
 
  -- Make body for functions `valid` and `invalid`:
  --   valid = oneof [ proper2 A | proper1 C]
  --   or
  --   valid = proper3 B, depending on the number of constructors
  validBody <- case constructors of
                    [c] -> normalB [| $(mkClause "proper" c) |]
                    cs  -> normalB [| oneof $(listE (map (mkClause "proper") cs)) |]
  invalidBody <- case constructors of
                      [c] -> normalB [| $(mkClause "bad" c) |]
                      cs  -> normalB [| oneof $(listE (map (mkClause "bad") cs)) |]
 
  -- Generate template instance declaration and replace type name (T1)
  --   and function body (x = "text") with our data
  d <- [d| instance Variant T3 where
             valid   = liftM T3 valid
             invalid = liftM T3 invalid
       |]
  let    [InstanceD [] (AppT showt (ConT _T3)) [ ValD validf _valid [], ValD invalidf _invalid [] ]] = d
  return [InstanceD [] (AppT showt (ConT t  )) [ ValD validf validBody [], ValD invalidf invalidBody [] ]]
 
-- -- Usage:
-- $(deriveVariant ''Record)
-- 
-- Adept
-- 
-- Retrieved from "http://haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator"
-- 
-- This page has been accessed 10,374 times. This page was last modified 03:22, 15 September 2009. Recent content is available under a simple permissive license.
-- 
-- Recent content is available under a simple permissive license.
-- 
