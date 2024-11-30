module Imports where

import Control.Arrow (ArrowChoice (left))
import Control.Monad (filterM, sequence)
import Data.Functor ((<&>))
import HumanLexer (GetTokenError, tokenize)
import HumanParser (HCompileError, HProgram, programFromTokens)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

data ImportError
  = ParserError HCompileError
  | LexerError GetTokenError
  | ModuleNotFound [String]
  | OverlappingModules [String] [FilePath]
  deriving (Show)

type ImportResult a = Either ImportError a

resolveImport :: [String] -> [FilePath] -> IO (ImportResult HProgram)
resolveImport modulePath importPaths = do
  filepath <- findModulePath modulePath importPaths
  case filepath of
    Left err -> return $ Left err
    Right filepath -> importFromFile filepath

importFromFile :: FilePath -> IO (ImportResult HProgram)
importFromFile file = readFile file <&> importFromContents

importFromContents :: String -> ImportResult HProgram
importFromContents contents = do
  tokens <- left LexerError $ tokenize contents
  left ParserError $ programFromTokens tokens

findModulePath :: [String] -> [FilePath] -> IO (ImportResult FilePath)
findModulePath modulePath paths = do
  possible <- matchModulePath modulePath paths
  case possible of
    [] -> return . Left $ ModuleNotFound modulePath
    [single] -> return $ Right single
    several -> return . Left $ OverlappingModules modulePath several

matchModulePath :: [String] -> [FilePath] -> IO [FilePath]
matchModulePath modulePath paths =
  let relativePath = joinModulePath modulePath
   in filterM doesFileExist $ map (</> relativePath) paths

joinModulePath :: [String] -> FilePath
joinModulePath path = foldr (flip (</>)) "." (reverse path) ++ ".human"