module HumanParser where

import Data.Map
import HumanLexer (HKeyword (..), HToken (..))

data HMacroArg = Str String | Exp HExp deriving (Eq, Show)

data HExp
  = Lambda String HExp
  | Apply HExp HExp
  | Id (Maybe String) String
  | MacroCall String [HMacroArg]
  deriving (Eq, Show)

data HStatement
  = Import [String] (Maybe String) (Maybe [(String, Maybe String)])
  | Def String HExp
  | Out String HExp
  deriving (Show)

data HProgram = HProgram (Maybe String) [HStatement] deriving (Show)

data HCompileError
  = UndefinedIdentifier String
  | UnresolvedImport String
  | UnexpectedKw HKeyword
  | UnexpectedSymbol String
  | -- Expected, got
    ExpectedToken HToken HToken
  | ExpectedExpression
  | ExpectedId
  | EmptyExpression
  | UnexpectedEOF
  | UnexpectedToken HToken
  | ErrMsg String
  deriving (Show)

programFromTokens :: [HToken] -> Either HCompileError HProgram
programFromTokens ((Keyword KwModule) : (IdStr name) : Semicolon : rest) = do
  statements <- programStatementsFromTokens rest
  return $ HProgram (Just name) statements
programFromTokens toks = do
  statements <- programStatementsFromTokens toks
  return $ HProgram Nothing statements

untilClosingParen :: (HToken -> Maybe HCompileError) -> [HToken] -> Either HCompileError ([HToken], [HToken])
untilClosingParen validate toks =
  let untilClosingParen' (RParen : rest) acc 0 = Right (reverse acc, rest)
      untilClosingParen' (RParen : rest) acc n = untilClosingParen' rest (RParen : acc) (n - 1)
      untilClosingParen' (LParen : rest) acc n = untilClosingParen' rest (LParen : acc) (n + 1)
      untilClosingParen' (tok : rest) acc n
        | Just err <- validate tok = Left err
        | otherwise = untilClosingParen' rest (tok : acc) n
   in untilClosingParen' toks [] 0

validateExpressionToken :: HToken -> Maybe HCompileError
validateExpressionToken (Keyword kw) = Just $ UnexpectedKw kw
validateExpressionToken Semicolon = Just $ UnexpectedSymbol ";"
validateExpressionToken LCurly = Just $ UnexpectedSymbol "{"
validateExpressionToken RCurly = Just $ UnexpectedSymbol "}"
validateExpressionToken _ = Nothing

-- Consumes all tokens and returns an error if it can't
convertToExpression :: [HToken] -> Either HCompileError HExp
convertToExpression [] = Left EmptyExpression
convertToExpression toks = do
  (exp, toks) <- getExpression' toks
  case toks of
    [] -> return exp
    toks -> do
      exp' <- convertToExpression toks
      return $ Apply exp exp'

getId :: [HToken] -> Either HCompileError (String, [HToken])
getId ((IdStr id) : rest) = Right (id, rest)
getId _ = Left ExpectedId

expectToken :: HToken -> [HToken] -> Either HCompileError [HToken]
expectToken expected (tok : rest)
  | expected == tok = Right rest
  | otherwise = Left $ ExpectedToken expected tok

getLambda :: [HToken] -> Either HCompileError (HExp, [HToken])
getLambda toks = do
  (name, toks) <- getId toks
  toks <- expectToken Dot toks
  (body, toks) <- getExpression toks
  return (Lambda name body, toks)

getMacroArg :: [HToken] -> Either HCompileError (HMacroArg, [HToken])
getMacroArg (Text val : rest) = Right (Str val, rest)
getMacroArg toks = do
  (exp, toks) <- getExpression' toks
  return (Exp exp, toks)

getMacroArgs :: [HToken] -> Either HCompileError ([HMacroArg], [HToken])
getMacroArgs (RBracket : toks) = Right ([], toks)
getMacroArgs toks = do
  (arg, toks) <- getMacroArg toks
  case toks of
    Comma : toks -> do
      (rest, toks) <- getMacroArgs toks
      return (arg : rest, toks)
    RBracket : toks -> Right ([arg], toks)

getMacroCall :: [HToken] -> Either HCompileError (HExp, [HToken])
getMacroCall toks = do
  (name, toks) <- getId toks
  toks <- expectToken LBracket toks
  (args, toks) <- getMacroArgs toks
  return (MacroCall name args, toks)

canStartExpression :: HToken -> Bool
canStartExpression Backslash = True
canStartExpression LParen = True
canStartExpression (IdStr _) = True
canStartExpression Hash = True
canStartExpression _ = False

getAdjacentExpressions :: [HToken] -> Either HCompileError ([HExp], [HToken])
getAdjacentExpressions [] = Right ([], [])
getAdjacentExpressions toks@(tok : _)
  | canStartExpression tok = do
      (exp, toks) <- getExpression' toks
      (rest, toks) <- getAdjacentExpressions toks
      return (exp : rest, toks)
  | otherwise = Right ([], toks)

joinApplication :: [HExp] -> Either HCompileError HExp
joinApplication [] = Left ExpectedExpression
joinApplication [exp] = Right exp
joinApplication (f : s : exps) = Right (Prelude.foldr (flip Apply) (Apply f s) $ reverse exps)

-- Get one expression (getExpression'), error otherwise.
-- Then, recursively get more expressions until the next token cannot be consumed.
getExpression :: [HToken] -> Either HCompileError (HExp, [HToken])
getExpression toks = do
  (exps, toks) <- getAdjacentExpressions toks
  exp <- joinApplication exps
  return (exp, toks)

getExpression' :: [HToken] -> Either HCompileError (HExp, [HToken])
getExpression' (Backslash : rest) = getLambda rest
getExpression' (LParen : rest) = do
  (toks, rest) <- untilClosingParen validateExpressionToken rest
  exp <- convertToExpression toks
  return (exp, rest)
getExpression' (IdStr qualifier : DoubleColon : rest) = do
  (name, rest) <- getId rest
  return (Id (Just qualifier) name, rest)
getExpression' (IdStr name : rest) = Right (Id Nothing name, rest)
getExpression' (Hash : rest) = getMacroCall rest

getDefinition :: [HToken] -> Either HCompileError (HStatement, [HToken])
getDefinition toks = do
  (name, toks) <- getId toks
  toks <- expectToken Equals toks
  (body, toks) <- getExpression toks
  toks <- expectToken Semicolon toks
  return (Def name body, toks)

getOutput :: [HToken] -> Either HCompileError (HStatement, [HToken])
getOutput toks = do
  (name, toks) <- getId toks
  toks <- expectToken Equals toks
  (body, toks) <- getExpression toks
  toks <- expectToken Semicolon toks
  return (Out name body, toks)

tryGet :: ([HToken] -> Either HCompileError (a, [HToken])) -> [HToken] -> Either HCompileError (Maybe a, [HToken])
tryGet getter toks = case getter toks of
  Left _ -> return (Nothing, toks)
  Right (result, toks) -> return (Just result, toks)

getTokenSeparated :: ([HToken] -> Either HCompileError (a, [HToken])) -> HToken -> [HToken] -> Either HCompileError ([a], [HToken])
getTokenSeparated getter separator toks = do
  (first, toks) <- getter toks
  (vals, toks) <- getTokenSeparated' toks []
  return (first : vals, toks)
  where
    getTokenSeparated' (s : toks) acc | s == separator = do
      (val, toks) <- getter toks
      getTokenSeparated' toks (val : acc)
    getTokenSeparated' toks acc = return (reverse acc, toks)

getImportName :: [HToken] -> Either HCompileError ([String], [HToken])
getImportName = getTokenSeparated getId Dot

getImportFields :: [HToken] -> Either HCompileError ([(String, Maybe String)], [HToken])
getImportFields toks = do
  (fields, toks) <- getTokenSeparated getImportField Comma toks
  toks <- expectToken RCurly toks
  return (fields, toks)
  where
    getImportField toks = do
      (name, toks) <- getId toks
      (qualifiedName, toks) <-
        tryGet
          ( \toks -> do
              toks <- expectToken (Keyword KwAs) toks
              (qualifiedName, toks) <- getId toks
              return (qualifiedName, toks)
          )
          toks
      return ((name, qualifiedName), toks)

getImport :: [HToken] -> Either HCompileError (HStatement, [HToken])
getImport toks = do
  (modulePath, toks) <- getImportName toks
  toks <- expectToken (Keyword KwAs) toks
  (qualifiedName, toks) <- tryGet getId toks
  case toks of
    LCurly : toks -> do
      (fields, toks) <- getImportFields toks
      toks <- expectToken Semicolon toks
      return (Import modulePath qualifiedName (Just fields), toks)
    toks -> do
      toks <- expectToken Semicolon toks
      return (Import modulePath qualifiedName Nothing, toks)

getStatement :: [HToken] -> Either HCompileError (HStatement, [HToken])
getStatement ((Keyword KwDef) : rest) = getDefinition rest
getStatement ((Keyword KwOut) : rest) = getOutput rest
getStatement ((Keyword KwImport) : rest) = getImport rest
getStatement (tok : _) = Left $ UnexpectedToken tok

programStatementsFromTokens :: [HToken] -> Either HCompileError [HStatement]
programStatementsFromTokens [EOF] = Right []
programStatementsFromTokens toks = do
  (statement, toks') <- getStatement toks
  rest <- programStatementsFromTokens toks'
  return $ statement : rest