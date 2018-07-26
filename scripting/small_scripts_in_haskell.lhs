Neil Mitchell's Haskell Blog
Tuesday, January 27, 2009
Small scripts with Haskell

Normally I give blog posts detailing the fun, interesting or advanced stuff I do with Haskell. But that isn't a real representation of my programming life! Most of the time I am doing small scripts that do little tasks, so I thought I'd describe one of those. This post is written as Literate Haskell, which means you can save the whole contents as a .lhs file and run it in GHCi or Hugs.

The task I had to complete was to take a directory of files, and for each file foo.txt generate the files foo_m1.txt to foo_m3.txt, where each one file is a block of lines from the original delimited by a blank line. i.e. given the file with the lines ["","1","1","","2","","3"], the numbers "1" would go in foo_m1.txt etc.

This blog post isn't how I actually wrote the original script - I didn't use literate Haskell (since I find it ugly), I didn't give explicit import lists (since they are needlessly verbose), I didn't give type signatures (but I should have) and I didn't split the IO and non-IO as well (but again, I should have). It is intended as a guide to the simple things you can easily do with Haskell. Now on to the code...


> import System.FilePath(takeExtension, dropExtension, (<.>), (</>))
> import System.Directory(getDirectoryContents)
> import Data.Char(isSpace)
> import Control.Monad



First, let's import some useful modules. To find more about a particular function just use Hoogle and search for it, but a quick summary:


takeExtension "foo.txt" = ".txt"
dropExtension "foo.txt" = "foo"
"foo" <.> "txt" = "foo.txt"
"bar" </> "foo.txt" = "bar/foo.txt"
getDirectoryContents "C:\Windows" = running "dir C:\Windows" at the command prompt
isSpace ' ' = True



Every Haskell program starts with a main function, which is an IO action. For this program, we are going to keep all the IO in main, and only use other pure functions. With most file processing applications its best to read files from one directory, and write them to another. That way, if anything goes wrong, its usually easy to recover. In this case we read from "data" and write to "res".


> main :: IO ()
> main = do
>     files <- getDirectoryContents "data"
>     forM_ files $ \file -> when (takeExtension file == ".txt") $ do
>         src <- readFile $ "data" </> file
>         forM_ (zip [1..] (splitFile src)) $ \(i,x) ->
>              writeFile ("res" </> dropExtension file ++ "_m" ++ show i <.> "txt") x



Or in some kind of pseudo-code:


main =
    set files to be the list of files in the directory "data"
    for each file in files which has the extension ".txt"
    {
        set src to be the result of reading the file
        for each numbered result of splitFile
        {
            write out the value from splitFile to the location "res/file_m#.txt"
            where # is the 1-based index into the list of results
        }
    }



We can now move on to the pure bits left over. We want a function splitFile that takes a file, and splits it in to three chunks for each of the blocks in the file. When processing text, often there will be stray blank lines, and the term "blank lines" will also apply to lines consisting only of spaces. The code is below:


> splitFile :: String -> [String]
> splitFile xs = map (tabify . unlines) [s1,s2,s3]
>     where
>         xs2 = dropWhile null $ map (dropWhile isSpace) $ lines xs
>         (s1,_:rest) = break null xs2
>         (s2,_:s3) = break null $ dropWhile null rest



And now presented more as a list of steps:


    * split the text in to lines

    * for each line drop all the leading spaces from it

    * drop all the leading blank lines

    * break on the first empty line, the bits before are chunk 1

    * drop all leading blank lines for the rest

    * break on the first empty line in the rest, before is chunk 2, after is chunk 3

    * for each of the chunks, put the lines back together, then tabify them



The tabify requirement was added after. The person decided that all continuous runs of spaces should be converted to tabs, so the file could better be loaded in to a spread sheet. Easy enough to add, just a simple bit of recursive programming:


> tabify (' ':xs) = '\t' : tabify (dropWhile (== ' ') xs)
> tabify (x:xs) = x : tabify xs
> tabify [] = []



And again in English:


    * if you encouter a space, drop it and all successive spaces, and write out a tab

    * otherwise just continue onwards



Haskell is a great language for writing short scripts, and as the libraries improve it just keeps getting better.
Posted by Neil Mitchell at 8:43 PM