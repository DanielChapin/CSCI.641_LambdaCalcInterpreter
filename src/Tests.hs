module Tests where

import DBIUTLC
import Execution (executeResult)
import Human
import Macros (namedMacros)

executeTest :: Bool -> Bool -> String -> IO ()
executeTest verbose eagerEnd test =
  let executor = case (verbose, eagerEnd) of
        (True, False) -> prettyExecuteFile
        (False, False) -> prettyExecuteFileResult
        (False, True) -> prettyExecuteFileResultClean
        -- TODO This isn't really useful for my demonstrations but it's here if someone wants to implement it lol
        (True, True) -> error "Unimplemented"
      testPath = "../human/interpreter/test/" ++ test ++ ".human"
      importPaths = ["../human/interpreter", "../human/stl"]
   in executor testPath importPaths namedMacros

variantsTest :: IO ()
variantsTest = executeTest False True "variants"

shiftTest :: IO ()
shiftTest = do
  print $ shift 1 0 (Var 0)
  executeTest False False "shift"

substituteTest :: IO ()
substituteTest = do
  print $ substitute 0 (Var 1) (Var 0)
  print $ substitute 1 (Var 1) (Abstraction "_" (Var 0))
  executeTest False True "substitute"

evaluationTest :: IO ()
evaluationTest = do
  print $ executeResult step (Abstraction "_" (Var 0))
  executeTest False True "programs"