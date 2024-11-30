module Macros where

import HumanParser (HExp (Id, Lambda, MacroCall), HMacroArg (Str))
import HumanTranspilerDBI (TranspilationError (TError), TranspilationErrorType (Other), TranspilationResult)
import Text.Read (readMaybe)

type MacroHandler = [HMacroArg] -> TranspilationResult HExp

namedMacros :: [(String, MacroHandler)]
namedMacros = [("id", idMacro), ("peano", peanoMacro)]

idMacro :: MacroHandler
idMacro [Str var] = return $ Lambda var (Id Nothing var)

peanoMacro :: MacroHandler
peanoMacro args@[Str num]
  | Just num <- readMaybe num :: Maybe Int = error "test"
  | otherwise = Left [TError Other Nothing (Just $ MacroCall "peano" args) Nothing]