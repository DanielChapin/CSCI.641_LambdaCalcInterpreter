module Main where

import HumanLexer (tokenize)
import HumanParser (HCompileError (ErrMsg), HProgram, programFromTokens)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  inPath <- case args of
    [path] -> return path
    _ -> fail "Expected exactly 1 input."
  result <- parseFile $ readFile inPath
  case result of
    Left err -> do putStrLn $ "A fatal error occured.\n" ++ show err
    Right prog -> do print prog

parseFile :: IO String -> IO (Either HCompileError HProgram)
parseFile file = do
  input <- file
  case tokenize input of
    Left err -> return . Left . ErrMsg $ "Error tokenizing input.\n" ++ show err
    Right toks -> return $ programFromTokens toks