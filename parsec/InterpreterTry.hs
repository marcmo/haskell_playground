-- imports
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (emptyDef)

-- | main
main :: IO ()
main = do

  -- get a line from stdin
  input <- getLine

  -- parse it
  case parse parse_calc "" input of
    Left er  -> putStrLn $ "Error: " ++ (show er)
    Right cl -> putStrLn (show cl)

  main

-- | function generating a token parser based on a
-- lexical parsers combined with a language record definition
lexer :: PT.TokenParser st
lexer  = PT.makeTokenParser emptyDef

-- | token parser for parenthesis
parens :: CharParser st a -> CharParser st a
parens = PT.parens lexer

-- | token parser for Integer
integer :: CharParser st Integer
integer = PT.integer lexer

-- | token parser for Double
float :: CharParser st Double
float = PT.float lexer

-- | token parser for Either Integer Double
naturalOrFloat :: CharParser st (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat lexer

-- | token parser for keywords
reservedOp :: String -> CharParser st ()
reservedOp = PT.reservedOp lexer

-- | helper function for defining real powers
real_exp :: Double -> Double -> Double
real_exp a x = exp $ x * log a

-- | grammar description for parser
parse_calc :: CharParser () Double
parse_calc = mul_term `chainl1` add_action

mul_term :: CharParser () Double
mul_term = exp_term `chainl1` multiply_action

exp_term :: CharParser () Double
exp_term = factor `chainl1` exp_action

factor :: CharParser () Double
factor = parens parse_calc
             <|> parse_sqr
             <|> parse_number

parse_sqr :: CharParser () Double
parse_sqr = reservedOp "sqrt" >> parens parse_calc >>=
                    \x -> return $ sqrt x

multiply_action :: CharParser () (Double -> Double -> Double)
multiply_action = do {reservedOp "*"; return (*) }
                            <|> do {reservedOp "/"; return (/) }

add_action :: CharParser () (Double -> Double -> Double)
add_action = do { reservedOp "+"; return (+) }
                      <|> do { reservedOp "-"; return (-) }

exp_action :: CharParser () (Double -> Double -> Double)
exp_action = reservedOp "^" >> return real_exp

parse_number :: CharParser () Double
parse_number = naturalOrFloat >>=
                            \num -> case num of
                           Left i  -> return $ fromInteger i
                           Right x -> return x

