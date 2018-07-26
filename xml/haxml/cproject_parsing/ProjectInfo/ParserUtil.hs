module ProjectInfo.ParserUtil
    (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    , eol
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- applicative parsec definitions
-- instance Applicative (GenParser s a) where
--     pure  = return
--     (<*>) = ap
-- 
-- instance Alternative (GenParser s a) where
--     empty = mzero
--     (<|>) = mplus

-- other parsing utilities
eol :: GenParser Char () String
eol = try (string "\r\n") <|> string "\n" <?> "EOL"
