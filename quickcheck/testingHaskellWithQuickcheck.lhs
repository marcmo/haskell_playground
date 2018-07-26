Testing Haskell with QuickCheck
After my last post about working with HUnit for doing unit testing in Haskell, dons suggested that I look at QuickCheck as an alternative that enables higher level testing.

QuickCheck is an easy to use framework that requires you to write a specification of your code by defining invariant properties. It then generates random, sample data to verify that the properties hold. This higher level view of testing is a good match with Haskell, since the functional nature makes unit testing far less important than with other languages. In comparison, the *Unit family of frameworks with their requirement to write individual tests with their associated data begin to feel like the assembly language of testing.

Note: This article is Literate Haskell and can be loaded directly into ghci. That's the good part, but a downside of using the literate approach is that functions will have to have a version number suffix to differentiate them as we do rewrites. Even so, I think the literate approach is still worth it. Furthermore, the article was developed using a build of QuickCheck-2.0 from a recent snapshot from the darcs repo (http://code.haskell.org/QuickCheck). Not all of the functions seen here are available in the current QuickCheck-1.1.0.0 release version.


> import Control.Monad ( liftM )
> import Data.List ( intersperse )
> import Test.QuickCheck
> import System.Random
>   ( RandomGen(..)
>   , Random(..)
>   , StdGen
>   , newStdGen
>   )

> vectorOf' :: Int -> Gen a -> Gen [a]
> vectorOf' k gen = sequence [ gen | _ <- [1..k] ]
> listOf' :: Gen a -> Gen [a]
> listOf' gen = sized $ \n ->
>   do k <- choose (0,n)
>      vectorOf' k gen

The best way to see what QuickCheck offers is to use an example. In this case we'll create a simple library that works with filenames. splitFN will split filenames into their name and extension, joinFN will turn a name and extension into a full filename. Here's a simple implementation of both.


> splitFN_0 :: String -> (String, String)
> splitFN_0 fn =
>   let fn' = span (/= '.') . reverse $ fn
>   in case (length (fst fn') == length fn) of
>        True  -> (fn, "")
>        False -> (reverse . drop 1 $ snd fn', ('.':) . reverse . fst $ fn')

> joinFN_0 :: (String, String) -> String
> joinFN_0 (name, ext) = name ++ ext


Since our library can split filenames and join them back together, a natural property for our code is where filenames are roundtripped. Since we need to generate useful filenames, we'll go ahead and define a newtype for them and we'll instance Arbitrary to generate the filenames.


> newtype Filename = FN { unFN :: String } deriving Show

> instance Arbitrary Filename where
>   arbitrary = do name <- elements ["foo", "bar", "baz"]
>                  ext <- listOf' $ elements ['a'..'z']
>                  return (FN (name ++ "." ++ ext))

> prop_filenames_are_roundtrippable_0 :: Filename -> Property
> prop_filenames_are_roundtrippable_0 fnStr =
>   property $ joinFN_0 (splitFN_0 fn) == fn
>   where fn = unFN fnStr


And when we load the code into ghci and run it ...

*Main> quickCheck prop_filenames_are_roundtrippable_0
+++ OK, passed 100 tests.


Okay so it passes all the tests. Great we're done ... well, not quite. We wrote code to generate the filenames, but what do they really look like? We can ask QuickCheck to show us some samples.

*Main> sample' arbitrary :: IO [Filename]

[ FN {unFN = "baz.x"}, FN {unFN = "bar.v"}, FN {unFN = "foo.k"}, FN {unFN = "foo.s"}, FN {unFN = "baz.esra"}, FN {unFN = "baz.vkgg"}, FN {unFN = "bar.uln"}, FN {unFN = "bar.k"}, FN {unFN = "baz.crynhi"}, FN {unFN = "baz.ys"} ]

We can also augment our property with collect to show us what data it is using.


> prop_filenames_are_roundtrippable_1 :: Filename -> Property
> prop_filenames_are_roundtrippable_1 fnStr =
>   collect fn $
>   joinFN_0 (splitFN_0 fn) == fn
>   where fn = unFN fnStr


Here's a subset of what it shows.

*Main> quickCheck prop_filenames_are_roundtrippable_1
 
 1% "bar.tbgufhjxeqtfpn"
 1% "bar.rymnlegngyuzvl"
 1% "bar.ryddfkncgxdopxihkmb"
 1% "bar.rafel"
 1% "bar.qbrftss"
 1% "bar.pxwpbfovejqwiqslnrdboaluihlkjifawfiyerwwtdyuepynoejx"
 1% "bar.p"
 1% "bar.nyciiyidegiwpsxta"
 1% "bar.mtidqpnitvrseakbkppkjmqtlutkqtfuirlsmkrnsmxsvwhzhwfut"
 1% "bar.mogmmzl"


Since collect may overwhelm us with too much info, we can alternatively classify the data.


> prop_filenames_are_roundtrippable_2 :: Filename -> Property
> prop_filenames_are_roundtrippable_2 fnStr =
>   classify (length ext == 0) "no ext" $
>   classify (length ext > 0 && length ext < 5) "normal ext" $
>   classify (length ext >= 5) "long ext" $
>   joinFN_0 (splitFN_0 fn) == fn
>   where fn = unFN fnStr
>         (name,ext) = splitFN_0 fn


*Main> quickCheck prop_filenames_are_roundtrippable_2
+++ OK, passed 100 tests:
72% long ext
21% normal ext
 7% no ext


After reviewing the test data being generated, it should be pretty clear that we're not really handling the full set of valid filenames that we might encounter. For example, we haven't considered filenames such as "README", "foo.txt.old", ".emacs" or even "foo.". Clearly we need to beef up our test data generator.

We're going to use this opportunity to see an alternate way that we can write a test generator. Rather than instancing Arbitrary, we can just write a generator and then use it in our property.


> filenames :: Gen String
> filenames = do
>   name <- opt identifier
>   dot  <- opt (return ".")
>   ext  <- opt identifier
>   exts <- listOf' identifier
>   oneof [ return $ name ++ dot ++ ext
>         , return $ name ++ "." ++ (concat . intersperse "." $ exts)]

> prop_filenames_are_roundtrippable_3 :: Property
> prop_filenames_are_roundtrippable_3 =
>   forAll filenames $ \fn ->
>   joinFN_0 (splitFN_0 fn) == fn


Sampling our test data generator, we can see that our filenames are much more varied and interesting now.

*Main> sample' filenames

[ ".K3", ".O.Va1", "1LAi.k", "rz.t", "41R8x.", ".wu.mi1kqh8.Y7PKH6.p86.O", "", ".", "P214MM71fu.k4Ayqns0f", ".k.k9.0o2e81n.d71ijpm7gh.XMNt" ]

*Main> quickCheck prop_filenames_are_roundtrippable_3
+++ OK, passed 100 tests.


And running our tests, everything passes. But since our test data is more interesting, our level of confidence has increased. Even so, we're not quite done yet as there is another property that we can put on our code.

Some files such as "README" and dotfiles such as ".emacs" don't have a file extension. So we can assert that their name is equivalent to their orginal filename, even after running them through the splitFN function. We'll create a custom generator for these 'no extension' files and add a new property.


> noExtFilenames :: Gen String
> noExtFilenames = do
>   name <- identifier
>   dot  <- opt (return ".")
>   return ( dot ++ name )

> prop_names_equal_filenames_0 :: Property
> prop_names_equal_filenames_0 =
>   forAll noExtFilenames $ \fn ->
>   let (name,ext) = splitFN_0 fn
>   in name == fn


Crikey, as we run through these tests we see that we have a bug in our split routine.

*Main> quickCheck prop_names_equal_filenames_0
*** Failed! Falsifiable (after 3 tests):  
".i1"

*Main> splitFN_0 ".i1"
("",".i1")


So we'll make a quick fix to our splitFN and update all the functions that use it and we're back in business. See the code at the end of this article for the updates and various library functions.

*Main> quickCheck prop_filenames_are_roundtrippable_4
+++ OK, passed 100 tests.
*Main> quickCheck prop_names_equal_filenames_1 
+++ OK, passed 100 tests.


So let's recap what we've done. We've created two custom filename generators, filenames and noExtFilenames. We've defined two properties on our code, prop_filenames_are_roundtrippable and prop_names_equal_filenames, that use those generators. And we've created some reusable library functions. In return for that minimal amount of work, QuickCheck will happily generate test data and verify our properties.

For the kind of testing we've done, there's no way that the *Unit family could compete on how little code you have to write. QuickCheck seems to offer a lot and will certainly be used on all my projects. But only time will tell whether it can fully replace the *Unit family or augment them.

In the mean time, let me say thanks to Koen Classen and John Hughes for giving us such a great tool.
Remaining Code


> splitFN_1 :: String -> (String, String)
> splitFN_1 fn =
>   let fn' = span (/= '.') . reverse $ fn
>   in case (length (fst fn') == length fn) of
>        True  -> (fn, "")
>        False | length (fst fn') == length fn - 1 -> (fn, "")
>              | otherwise -> (reverse . drop 1 $ snd fn'
>                             , ('.':) . reverse . fst $ fn')

> prop_names_equal_filenames_1 :: Property
> prop_names_equal_filenames_1 =
>   forAll noExtFilenames $ \fn ->
>   let (name,ext) = splitFN_1 fn
>   in name == fn

> prop_filenames_are_roundtrippable_4 :: Property
> prop_filenames_are_roundtrippable_4 =
>   forAll filenames $ \fn ->
>   joinFN_0 (splitFN_1 fn) == fn

> ----------------------------
> -- library functions

> iden0 :: Gen Char
> iden0 = oneof [ elements ['a'..'z'], elements ['A'..'Z']
>               , elements ['0'..'9'] ]
> idenN :: Gen String
> idenN = listOf' iden0

> opt :: Gen String -> Gen String
> opt g = oneof [ g, return "" ]

> identifier :: Gen String
> identifier = iden0 >>= \i0 -> idenN >>= return . (i0:)


