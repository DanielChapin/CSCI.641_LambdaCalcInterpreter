module HumanTranspiler where

import DBIUTLC (DBILExp)
import Data.Map (Map)
import HumanParser (HExp, HProgram (HProgram), HStatement (Def, Out), isDefStatement, isOutStatement)

-- Output names to expressions
data Context = Context [(String, HExp)]

data TranspilationErrorType
  = UndefinedSymbol String
  | UnresolvedImport String
  deriving (Show)

data TranspilationError = TError TranspilationErrorType HStatement (Maybe String) deriving (Show)

type TranspilationResult a = Either [TranspilationError] a

-- Takes a program and generates pairs of output names and the cooresponding output expression (un-evaluated).
-- Steps of transpilation
--  1. Find all the output statements.
--  2. Recursively get all the required identifiers and their sources (ie. the current program or an import or recursion)
--  3. Validate that those requirements are present. (This involves finding imported modules and parsing them)
--  4. Recursively generate all the required definitions for all of the outputs, reusing anything possible.
--  5. Pack the defined outputs and return those values.
transpile :: HProgram -> TranspilationResult [(String, DBILExp)]
transpile program@(HProgram moduleName statements) =
  let outs = map (\(Out name body) -> (name, body)) $ filter isOutStatement statements
      defs = map (\(Def name body) -> (name, body)) $ filter isDefStatement statements
      context = Context outs
   in transpile' context program

transpile' :: Context -> HProgram -> TranspilationResult [(String, DBILExp)]
transpile' (Context outs) (HProgram moduleName statements) = error "Unimplemented"