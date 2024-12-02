module Human where

import Control.Arrow (Arrow (second), ArrowChoice (left))
import DBIUTLC (DBILExp, step, stepEager)
import Execution (execute, executeResult)
import HumanLexer (GetTokenError, tokenize)
import HumanParser (HCompileError (ErrMsg), HExp, HMacroArg, HProgram (HProgram), programFromTokens, showStatementTree)
import HumanTranspilerDBI (TranspilationError, TranspilationResult, transpile)
import System.Environment (getArgs)

data HumanError = LexerErr GetTokenError | ParserError HCompileError | TranspilerError [TranspilationError] deriving (Show)

parseFile :: String -> IO (Either HumanError HProgram)
parseFile file = do
  input <- readFile file
  case tokenize input of
    Left err -> return . Left . LexerErr $ err
    Right toks -> return . left ParserError $ programFromTokens toks

prettyParseFile :: String -> IO ()
prettyParseFile file = do
  result <- parseFile file
  case result of
    Left err -> print err
    Right (HProgram name statements) -> do
      putStrLn $ "module (" ++ show name ++ ")"
      pretty statements
  where
    pretty [] = putStr "\n"
    pretty (statement : rest) = do
      putStrLn $ showStatementTree statement
      pretty rest

transpileFile :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO (Either HumanError [(String, DBILExp)])
transpileFile file importPaths macros = do
  input <- parseFile file
  case input of
    Left err -> return $ Left err
    Right prog -> do
      result <- transpile prog importPaths macros
      return $ left TranspilerError result

prettyTranspileFile :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO ()
prettyTranspileFile file importPaths macros = do
  result <- transpileFile file importPaths macros
  case result of
    Left err -> print err
    Right result -> prettyPrintPairs $ reverse result

executeFile :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO (Either HumanError [(String, [DBILExp])])
executeFile file importPaths macros = do
  result <- transpileFile file importPaths macros
  case result of
    Left err -> return $ Left err
    Right outs -> return . Right $ map (second $ execute DBIUTLC.step) outs

prettyExecuteFile :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO ()
prettyExecuteFile file importPaths macros = do
  result <- executeFile file importPaths macros
  case result of
    Left err -> print err
    Right result -> prettyPrintNamedLists $ reverse result

executeFileResult :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO (Either HumanError [(String, DBILExp)])
executeFileResult file importPaths macros = do
  result <- transpileFile file importPaths macros
  case result of
    Left err -> return $ Left err
    Right outs -> return . Right $ map (second $ executeResult DBIUTLC.step) outs

prettyExecuteFileResult :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO ()
prettyExecuteFileResult file importPaths macros = do
  result <- executeFileResult file importPaths macros
  case result of
    Left err -> print err
    Right result -> prettyPrintPairs $ reverse result

executeFileResultClean :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO (Either HumanError [(String, DBILExp)])
executeFileResultClean file importPaths macros = do
  result <- transpileFile file importPaths macros
  case result of
    Left err -> return $ Left err
    Right outs -> return . Right $ map (second $ executeResult DBIUTLC.stepEager . executeResult DBIUTLC.step) outs

prettyExecuteFileResultClean :: String -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO ()
prettyExecuteFileResultClean file importPaths macros = do
  result <- executeFileResultClean file importPaths macros
  case result of
    Left err -> print err
    Right result -> prettyPrintPairs $ reverse result

prettyPrintPairs :: Show a => [(String, a)] -> IO ()
prettyPrintPairs pairs =
  let len = maximum $ map (length . fst) pairs
      pretty [] = return ()
      pretty ((name, val) : rest) =
        do
          putStrLn $ replicate (len - length name) ' ' ++ name ++ ": " ++ show val
          pretty rest
   in pretty pairs

prettyPrintNamedLists :: Show a => [(String, [a])] -> IO ()
prettyPrintNamedLists pairs =
  let len = maximum $ map (length . fst) pairs
      prettyList _ [] = return ()
      prettyList start (val : rest) =
        do
          putStr start
          print val
          prettyList start rest
      pretty [] = return ()
      pretty ((name, list) : rest) =
        do
          putStrLn $ replicate (len - length name) ' ' ++ name ++ ": " ++ show (head list)
          prettyList (replicate len ' ' ++ ": ") (tail list)
          pretty rest
   in pretty pairs
