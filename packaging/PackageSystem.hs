module PackageSystem 

where
import Data.List
import qualified Data.Map as M

type PackageId = String
type Registry = M.Map PackageId SoftwarePackage

data SoftwarePackage = MkPackage {
  packageId     :: PackageId,
  version       :: Version,
  dependencies  :: [Dependency]
} deriving (Eq)
instance Show SoftwarePackage
  where show (MkPackage id v ds) =
          "id: " ++ id ++ "\n" ++
          "version: " ++ show v ++ "\n" ++
          "dependencies: " ++ intercalate "," (map show ds)

shortDep (MkPackage id v _) = id ++ " [" ++ show v ++ "]"

data Dependency = Dep {
  dependentPackageId  :: PackageId,
  dependentVersion    :: Version
} deriving (Ord,Eq)
instance Show Dependency where
  show (Dep id v) = id ++ "==" ++ show v

data Version = Version Int Int Int
  deriving (Eq,Ord)
instance Show Version where
  show (Version x y z) = show x ++ "." ++ show y ++ "." ++ show z
  
toDep :: SoftwarePackage -> Version -> Dependency
toDep p v = Dep (packageId p) v
makeDeps :: [(PackageId,Version)] -> [Dependency]
makeDeps xs = map (\(x,y)->Dep x y) xs

common = MkPackage "common" (Version 2 1 0) []
eeprom = MkPackage "eeprom" (Version 1 0 0) [Dep "common"(Version 1 0 0)]
dem = MkPackage "dem" (Version 1 0 0) (makeDeps [("common",Version 1 0 0),("eeprom",Version 2 0 0)])

allPackages = toMap [common,eeprom,dem]

toMap :: [SoftwarePackage] -> Registry
toMap ps = M.fromList (map (\x->(packageId x,x)) ps)


