
import qualified Data.Set as S

data Test = Go | NoGo deriving (Eq)

foo :: [Test] -> S.Set Test
foo xs = S.fromList xs
