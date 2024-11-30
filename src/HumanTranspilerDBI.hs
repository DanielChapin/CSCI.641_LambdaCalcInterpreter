module HumanTranspilerDBI where

import Consts (humanYCombinator)
import Control.Arrow (Arrow (second))
import DBIUTLC (DBILExp (Abstraction, Application, Var))
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map (Map, empty, fromList, insert, member, union)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.IO (unsafePerformIO)
import Helpers (selectErrorsElseValues)
import HumanParser (HExp (..), HMacroArg (..), HProgram (HProgram), HStatement (Def, Import, Out), isDefStatement, isOutStatement)
import Imports (ImportError, findModulePath, joinModulePath, resolveImport)

data Context = Context
  { outs :: [(String, HExp)],
    defs :: Map String HExp,
    -- Key: alias/name; Value: (module path, name)
    importedSymbols :: Map String ([String], String),
    importedQualifiers :: Map String [String],
    varStack :: [String],
    importPaths :: [FilePath],
    modules :: Map [String] (Map String HExp),
    modulePaths :: [[String]],
    applyTranspilationMacro :: String -> [HMacroArg] -> TranspilationResult HExp
  }

macroHandler :: [(String, [HMacroArg] -> TranspilationResult HExp)] -> String -> [HMacroArg] -> TranspilationResult HExp
macroHandler handlers name args
  | Just handler <- lookup name handlers = handler args
  | otherwise = Left [TError (MacroNotFound name) Nothing (Just $ MacroCall name args) Nothing]

emptyContext :: Context
emptyContext =
  Context
    { outs = [],
      defs = empty,
      importedSymbols = empty,
      importedQualifiers = empty,
      varStack = [],
      importPaths = ["."],
      modules = empty,
      modulePaths = [],
      applyTranspilationMacro = \macroName args -> Left [TError Other Nothing (Just $ MacroCall macroName args) (Just "No macro handler supplied.")]
    }

data TranspilationErrorType
  = UndefinedSymbol String
  | UndefinedVariable String
  | UnresolvedImport String
  | OverlappingName String
  | ImportFailure [ImportError]
  | MacroNotFound String
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
transpile :: HProgram -> [FilePath] -> [(String, [HMacroArg] -> TranspilationResult HExp)] -> IO (TranspilationResult [(String, DBILExp)])
transpile program@(HProgram moduleName statements) importPaths macros =
  -- Look through all the statements and gather all important information
  case destructureStatements statements (emptyContext {importPaths, applyTranspilationMacro = macroHandler macros}) of
    Left err -> return $ Left err
    Right context -> do
      -- Resolve all module imports
      importContext <- resolveImports context
      case importContext of
        Left err -> return $ Left err
        Right context@(Context {outs}) ->
          return . transpile' $ context {outs = map (\(name, body) -> (name, encodeRecursion name body)) outs}

resolveImports :: Context -> IO (TranspilationResult Context)
resolveImports context@(Context {modulePaths, importPaths, modules}) = do
  imports <- mapM (flip resolveImport importPaths) modulePaths
  case selectErrorsElseValues imports of
    Left errs -> return $ Left [TError (ImportFailure errs) Nothing Nothing Nothing]
    Right imports ->
      let lookups = selectErrorsElseValues $ map (\(HProgram _ statements) -> moduleLookup statements) imports
       in case lookups of
            Left errLists -> return . Left $ concat errLists
            Right mappings -> return $ Right context {modules = modules `Map.union` fromList (zip modulePaths mappings)}
  where
    moduleLookup statements = do
      context@(Context {defs}) <- destructureStatements statements emptyContext
      return defs

transpile' :: Context -> TranspilationResult [(String, DBILExp)]
transpile' (Context {outs = []}) = return []
transpile' context@(Context {outs = ((name, out) : remaining)}) = do
  (context, out) <- transpileExp (context {outs = remaining}) out
  rest <- transpile' context
  return $ (name, out) : rest

transpileExp :: Context -> HExp -> TranspilationResult (Context, DBILExp)
transpileExp context@(Context {varStack}) (Lambda param body) = do
  (context', body) <- transpileExp (context {varStack = param : varStack}) body
  return (updateContext context context', Abstraction param body)
transpileExp context@(Context {varStack, defs, importedSymbols, importedQualifiers, modules}) exp@(Id qualifier name)
  | Just index <- displayName qualifier name `elemIndex` varStack =
      return (context, Var $ toInteger index)
  | (Nothing, Just def) <- (qualifier, Map.lookup name defs) = do
      (context', result) <- transpileExp context def
      return (updateContext context context', result)
  -- Resulting to checking imports for the name.
  -- We first need to see if it matches an import, then we need to validate that it's in the module.
  | (Nothing, Just (modulePath, defName)) <- (qualifier, Map.lookup name importedSymbols) = do
      def <- getDefModule context modulePath defName
      (context', result) <- transpileExp context def
      return (updateContext context context', result)
  | Just q <- qualifier,
    Just modulePath <- Map.lookup q importedQualifiers,
    Just mod <- Map.lookup modulePath modules,
    Just def <- Map.lookup name mod = do
      (context', result) <- transpileExp context def
      return (updateContext context context', result)
  | otherwise = Left [TError (UndefinedVariable $ displayName qualifier name) Nothing (Just exp) Nothing]
transpileExp context (Apply l r) = do
  (context', l) <- transpileExp context l
  (context', r) <- transpileExp (updateContext context context') r
  return (updateContext context context', Application l r)
transpileExp context@(Context {applyTranspilationMacro}) (MacroCall name args) = do
  exp <- applyTranspilationMacro name args
  (context', result) <- transpileExp context exp
  return (updateContext context context', result)

getDefModule :: Context -> [String] -> String -> TranspilationResult HExp
getDefModule (Context {modules}) modulePath defName
  | Just defs <- Map.lookup modulePath modules =
      case Map.lookup defName defs of
        Just def -> return def
        Nothing -> Left [TError (UndefinedSymbol defName) Nothing Nothing (Just $ "Couldn't find symbol '" ++ defName ++ "' in module " ++ joinModulePath modulePath)]
  | otherwise = Left [TError (UnresolvedImport $ joinModulePath modulePath) Nothing Nothing (Just "Tried to get def from unimported module.")]

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
isExpRecursive name (Id q v) = isNothing q && name == v
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
destructureStatements (s@(Import path maybeQualifier maybeFields) : rest) acc@(Context {modulePaths, importedSymbols, importedQualifiers})
  | path `elem` modulePaths =
      gatherErrors
        [TError (OverlappingName $ joinModulePath path) Nothing Nothing (Just "Overlapping import paths.")]
        (destructureStatements rest acc)
  -- Imports are a bit more tricky.
  -- We're always going to add the module path to the context.
  -- If we have fields, we will add those to the importedSymbols.
  -- The keys in importedSymbols are the actual symbols that can be referenced in the code.
  -- The values are the module path of the owner of the definition and the name in that module.
  -- If we have an qualifier for this alias, we will add that to the importedQualifiers.
  | otherwise =
      destructureStatements
        rest
        ( acc
            { modulePaths = path : modulePaths,
              importedSymbols = case maybeFields of
                Just fields ->
                  let symbols = map (\(defName, maybeAlias) -> (fromMaybe defName maybeAlias, (path, defName))) fields
                   in -- TODO Checking for overlapping aliases
                      importedSymbols `union` fromList symbols
                Nothing -> importedSymbols,
              importedQualifiers = case maybeQualifier of
                Just qualifier -> Map.insert qualifier path importedQualifiers
                Nothing -> importedQualifiers
            }
        )

gatherErrors :: [TranspilationError] -> TranspilationResult a -> TranspilationResult a
gatherErrors errs (Left errs') = Left $ errs ++ errs'
gatherErrors errs _ = Left errs