-- | A simple form of matching expression, given as a string
import Data.List

type GlobPattern = String
 
-- | Match a pattern element
pattern :: Char -> String -> [String]
pattern '?' (a:z) = [z]
pattern '?'   []  = []
pattern '*'    s  = tails s
pattern '+' (a:z) = tails z
pattern '+'   []  = []
pattern  c     s  = literal c s

-- | Match a literal pattern character exactly
literal :: Char -> String -> [String]
literal c (a:z) | a == c = [z]
literal _    _           = []

-- | Match a pattern to an input in all possible ways.
-- Return if it matched exactly the whole input for each way.
match :: GlobPattern -> String -> [Bool]
match ('\\':a:z) s = literal a s >>= match z
match (     a:z) s = pattern a s >>= match z
match        []  s = [null s]

-- | Test if a "glob" pattern matches the whole input
glob :: GlobPattern -> String -> Bool
glob p = or . match p

'?'%(a:z)=[z]
'*'%s=s:'+'%s
'+'%(a:z)='*'%z
c%s=c&s
c&(a:z)|a==c=[z]
_&_=[]
m('\\':a:z)s=a&s>>=m z
m(a:z)s=a%s>>=m z
m[]s=[null s]
g=(or.).m


