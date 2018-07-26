module Config

where

import System.FilePath

baseGitPath = joinDrive "/" $ joinPath ["home","omueller","dev","git"] -- linux
-- baseGitPath = joinDrive "/" $ joinPath ["Volumes","macbox_cs","dev","git"] -- mac
remoteRepo = baseGitPath </> "dispac" </> "remote"
localRepo = baseGitPath </> "dispac" </> "local"
