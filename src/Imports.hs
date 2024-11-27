module Imports where

import Control.Monad (filterM, sequence)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

matchModulePath :: [String] -> [FilePath] -> IO [FilePath]
matchModulePath modulePath paths =
  let relativePath = joinModulePath modulePath
   in filterM doesFileExist $ map (</> relativePath) paths

joinModulePath :: [String] -> FilePath
joinModulePath path = foldr (flip (</>)) "." (reverse path) ++ ".human"