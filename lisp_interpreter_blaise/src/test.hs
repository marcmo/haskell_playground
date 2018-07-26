import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


{-
Leftassociative: +, -, *, /
Rightassociative: ^
Priority 1: +,-
Priority 2: *, /
Priority 3: ^

expr   ::= factor (op factor)*
factor ::= number | '(' expr ')'

op     ::= '+' | '-' | '*' | '/' | '^'

number ::= ('0' | '1' | ... | '9')+
-}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        (emptyDef
        { commentStart = "/*",
          commentEnd = "*/",
          commentLine = "//",
          reservedOpNames = ["*","/","+","-", "^"]
        })

whiteSpace= P.whiteSpace lexer
natural = P.natural lexer
parens = P.parens lexer
reservedOp = P.reservedOp lexer

gram   = do  whiteSpace
             x <- expr
             eof
             return x

expr :: Parser Integer
expr = buildExpressionParser table factor
       <?> "_expression_"

-- Earlier in the list means a higher priority
table = [[op "^" (^) AssocRight]
        ,[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
        ]
       where op s f assoc = Infix (do{ reservedOp s; return f} <?> "operator") assoc
                           

factor = natural
         <|>
         (parens expr
          <?> "factor")

run :: Show a => Parser a -> String -> IO ()
run p input
       = case (parse p "" input) of
           Left err -> do{ putStr "parse error at "
                        ; print err
                        }
           Right x -> print x

exampleRun = run gram "(10 ^3 - /* comment 35 */(1 + 3))"
