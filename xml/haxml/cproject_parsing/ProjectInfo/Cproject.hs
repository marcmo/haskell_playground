module ProjectInfo.Cproject
where

import ProjectInfo.Parsing(parseProjectName)

data CProject = CProject {
  absolutPath :: FilePath,
  projectName :: String,
  configs :: [Configuration]
}
  
data Configuration = Configuration {
  name :: String,
  includePaths :: [String]
} 

data ProjectDependency = ProjectDependency {
  getProject :: String,
  dependents :: [String]
}

instance Show ProjectDependency where
  show (ProjectDependency n ds) = n ++ ":\n" ++ show ds

instance Show Configuration where
  show c = "config: " ++ name c ++ "\n" ++ (unlines depends)
    where depends = map (\(x,y)-> " -- " ++ x ++ ", ---- " ++ y) (zip ips shortnames)
          ips = includePaths c 
          shortnames = map shortname ips
          shortname p = shortFromEither $ parseProjectName p
          shortFromEither (Right x) = x
          shortFromEither _ = "no shortname found"

instance Show CProject where
  show p = "name: " ++ (projectName p) ++ "\n" 
    ++ "path: " ++ (absolutPath p) ++ "\n"
    ++ (unlines $ map show (configs p))

