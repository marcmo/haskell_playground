{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.HTML.Chunks
import System.IO (putStr, getLine, hFlush, stdout)

$(chunksFromFile "template01.html")

main :: IO ()
main = do { putStr "enter start int: "
          ; hFlush stdout
          ; start <- getLine
          ; putStr "end ending int: "
          ; hFlush stdout
          ; end <- getLine
          ; putStrLn (makePage (read start) (read end))
          }

makePage :: Int -> Int -> String
makePage low high
    | low >= high = format emptyPage
    | otherwise = format tablePage
    where
      emptyPage = Chunk_page { page_table = "" }
      tablePage = Chunk_page { page_table = buildTable [low..high] }

buildTable :: [Int] -> String
buildTable range = format (Chunk_table { table_rows = r })
    where
      r = (foldr (++) []) . map (\n -> format (makeRow n)) $ range
      makeRow :: Int -> Chunk_row
      makeRow n = Chunk_row { row_x = show n,
                              row_x_two = show (n * 2),
                              row_x_squared = show (n ^ 2),
                              row_x_xd = show (n ^ n)
                            }
