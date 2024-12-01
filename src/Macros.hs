module Macros where

import Consts
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
  -- TODO Encoding peano numerals from integers
  | Just num <- readMaybe num :: Maybe Int =
      let peano 0 = humanPair humanFalse humanId
          peano n = humanPair humanTrue (peano (n - 1))
       in return $ peano num
  | otherwise = Left [TError Other Nothing (Just $ MacroCall "peano" args) (Just "Peano numerals can only represent non-negative integers")]