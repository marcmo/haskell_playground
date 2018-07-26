module ParserUtil
    (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    , eol
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- other parsing utilities
eol :: GenParser Char () String
eol = try (string "\r\n") <|> string "\n" <?> "EOL"
