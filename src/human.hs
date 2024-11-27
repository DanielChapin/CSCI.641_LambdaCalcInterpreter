module Main where

import Control.Arrow (ArrowChoice (left))
import DBIUTLC (DBILExp)
import HumanLexer (GetTokenError, tokenize)
import HumanParser (HCompileError (ErrMsg), HExp, HProgram, programFromTokens)
import HumanTranspilerDBI (TranspilationError, transpile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  inPath <- case args of
    [path] -> return path
    _ -> fail "Expected exactly 1 input."
  result <- parseFile inPath
  case result of
    Left err -> do putStrLn $ "A fatal error occured.\n" ++ show err
    Right prog -> do print prog

data HumanError = LexerErr GetTokenError | ParserError HCompileError | TranspilerError [TranspilationError] deriving (Show)

parseFile :: String -> IO (Either HumanError HProgram)
parseFile file = do
  input <- readFile file
  case tokenize input of
    Left err -> return . Left . LexerErr $ err
    Right toks -> return . left ParserError $ programFromTokens toks

transpileFile :: String -> IO (Either HumanError [(String, DBILExp)])
transpileFile file = do
  input <- parseFile file
  case input of
    Left err -> return $ Left err
    Right prog -> return $ left TranspilerError $ transpile prog

prettyTranspileFile :: String -> IO ()
prettyTranspileFile file = do
  result <- transpileFile file
  case result of
    Left err -> print err
    Right result ->
      let pretty [] = do return ()
          pretty ((name, exp) : rest) = do putStrLn $ name ++ ": " ++ show exp; pretty rest
       in pretty $ reverse result