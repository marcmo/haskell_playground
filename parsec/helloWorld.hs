module Main
    where

import Text.ParserCombinators.Parsec

run :: Show a => Parser a -> String -> IO ()
run p input
        = case parse p "" input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

--simple:: Char
simple = letter

openClose :: Parser Char
openClose = do{ char '('
              ; char ')'
              }

parens  :: Parser ()
parens  = do{ char '('
            ; parens
            ; char ')'
            ; parens
            }
        <|> return ()

nesting :: Parser Int
nesting = do{ char '('
            ; n <- nesting
            ; char ')'
            ; m <- nesting
            ; return (max (n+1) m)
            }
          <|> return 0
