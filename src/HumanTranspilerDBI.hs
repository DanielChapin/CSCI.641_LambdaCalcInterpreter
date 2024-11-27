module HumanTranspilerDBI where

import Consts (humanYCombinator)
import Control.Arrow (Arrow (second))
import DBIUTLC (DBILExp (Abstraction, Application, Var))
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map (Map, empty, fromList, insert, member, union)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import HumanParser (HExp (..), HMacroArg (..), HProgram (HProgram), HStatement (Def, Import, Out), isDefStatement, isOutStatement)

data Context = Context
  { outs :: [(String, HExp)],
    defs :: Map String HExp,
    -- Key: (alias/name, qualifier); Value: (module path, name)
    imports :: Map (String, Maybe String) ([String], String),
    varStack :: [String],
    currentDef :: Maybe String,
    modules :: Map [String] (Map String HExp),
    applyTranspilationMacro :: String -> [HMacroArg] -> TranspilationResult HExp
  }

emptyContext :: Context
emptyContext =
  Context
    { outs = [],
      defs = empty,
      imports = empty,
      varStack = [],
      currentDef = Nothing,
      modules = empty,
      applyTranspilationMacro = \macroName args -> Left [TError Other Nothing (Just $ MacroCall macroName args) (Just "No macro handler supplied.")]
    }

data TranspilationErrorType
  = UndefinedSymbol String
  | UndefinedVariable String
  | UnresolvedImport String
  | OverlappingName String
  | Other
  deriving (Show)

data TranspilationError = TError TranspilationErrorType (Maybe HStatement) (Maybe HExp) (Maybe String) deriving (Show)

type TranspilationResult a = Either [TranspilationError] a

-- Takes a program and generates pairs of output names and the cooresponding output expression (un-evaluated).
-- Steps of transpilation
--  1. Find all the output statements.
--  2. Recursively get all the required identifiers and their sources (the current program or an import or recursion)
--  3. Validate that those requirements are present. (This involves finding imported modules and parsing them)
--  4. Recursively generate all the required definitions for all of the outputs, reusing anything possible.
--  5. Pack the defined outputs and return those values.
transpile :: HProgram -> TranspilationResult [(String, DBILExp)]
transpile program@(HProgram moduleName statements) = do
  context@(Context {outs}) <- destructureStatements statements emptyContext
  transpile' $ context {outs = map (\(name, body) -> (name, encodeRecursion name body)) outs}

transpile' :: Context -> TranspilationResult [(String, DBILExp)]
transpile' (Context {outs = []}) = return []
transpile' context@(Context {outs = ((name, out) : remaining)}) = do
  (context, out) <- transpileExp (context {outs = remaining}) out
  rest <- transpile' context
  return $ (name, out) : rest

transpileExp :: Context -> HExp -> TranspilationResult (Context, DBILExp)
transpileExp context@(Context {varStack}) exp@(Lambda param body) = do
  (context', body) <- transpileExp (context {varStack = param : varStack}) body
  return (updateContext context context', Abstraction param body)
transpileExp context@(Context {varStack, defs}) exp@(Id qualifier name)
  | Just index <- displayName qualifier name `elemIndex` varStack =
      return (context, Var $ toInteger index)
  | (True, Just def) <- (isNothing qualifier, Map.lookup name defs) = do
      (context', result) <- transpileExp context def
      return (updateContext context context', result)
  | otherwise = Left [TError (UndefinedVariable $ displayName qualifier name) Nothing (Just exp) Nothing]
transpileExp context (Apply l r) = do
  (context', l) <- transpileExp context l
  (context', r) <- transpileExp (updateContext context context') r
  return (updateContext context context', Application l r)
transpileExp context exp = error $ "Couldn't transpile exp: " ++ show exp

-- Update an old context with the globally accessible updates in another context
updateContext :: Context -> Context -> Context
updateContext old new@(Context {modules}) = old {modules}

encodeRecursion :: String -> HExp -> HExp
encodeRecursion name body
  | not $ isExpRecursive name body = body
  | otherwise =
      let recName = name ++ "*"
          substituteMacroArg (Exp exp) = Exp $ substituteAll exp
          substituteMacroArg arg = arg
          substituteAll exp@(Lambda param body)
            | param /= name = Lambda param $ substituteAll body
          substituteAll (Apply l r) = Apply (substituteAll l) (substituteAll r)
          substituteAll (Id Nothing name')
            | name == name' = Apply (Id Nothing recName) (Id Nothing recName)
          substituteAll (MacroCall macro args) = MacroCall macro $ map substituteMacroArg args
          substituteAll exp = exp
       in Apply humanYCombinator $ Lambda recName (substituteAll body)

isExpRecursive :: String -> HExp -> Bool
isExpRecursive name (Lambda param body)
  | param == name = False
  | otherwise = isExpRecursive name body
isExpRecursive name (Id Nothing v) = name == v
isExpRecursive name (Apply l r) = isExpRecursive name l || isExpRecursive name r
isExpRecursive name (MacroCall _ args) = any isArgRecursive args
  where
    isArgRecursive (Str _) = False
    isArgRecursive (Exp exp) = isExpRecursive name exp

displayName :: Maybe String -> String -> String
displayName (Just qualifier) name = qualifier ++ "::" ++ name
displayName Nothing name = name

-- Destructure all of the statements in a program for quick lookup.
-- Does NOT resolve any imports, but does check for unresolvable overlapping names.
destructureStatements :: [HStatement] -> Context -> TranspilationResult Context
destructureStatements [] acc = return acc
destructureStatements (s@(Out name body) : rest) acc@(Context {outs})
  | Just _ <- lookup name outs =
      gatherErrors
        [TError (OverlappingName name) (Just s) Nothing (Just $ "Output with name '" ++ name ++ "' already exists.")]
        (destructureStatements rest acc)
  | otherwise = destructureStatements rest (acc {outs = (name, body) : outs})
destructureStatements (s@(Def name body) : rest) acc@(Context {defs})
  | name `member` defs =
      gatherErrors
        [TError (OverlappingName name) (Just s) Nothing (Just $ "Definition with name '" ++ name ++ "' already exists.")]
        (destructureStatements rest acc)
  | otherwise = destructureStatements rest (acc {defs = insert name body defs})

gatherErrors :: [TranspilationError] -> TranspilationResult a -> TranspilationResult a
gatherErrors errs (Left errs') = Left $ errs ++ errs'
gatherErrors errs _ = Left errs