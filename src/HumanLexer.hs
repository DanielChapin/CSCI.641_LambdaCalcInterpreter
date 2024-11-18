module HumanLexer where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Char (isAlpha, isAlphaNum, isSpace)

-- Comment ::= --<string>\n     (Implicitly interspersed)
--
-- Program ::= module <name>; Statement*
--         ::= Statement*
--
-- Statement ::= ImportStatement
--           ::= Def | Out
--
-- ImportStatement ::= import <name>(.<name>)* as Import;
-- Import ::= <name> | ImportFields | <name> ImportFields
-- ImportFields ::= { ImportField(, ImportField)* }
-- ImportField ::= <name> | <name> as <name>
--
-- Def ::= def <name> = Exp;
-- Out ::= out <name> = Exp;
--
-- Exp ::= \<name>. Exp
--     ::= (Exp) | Exp Exp
--     ::= <name> | <name>::<name>
--     ::= #<name>[<string> | Exp | <nothing>]
--
-- <name> ::= [a-zA-Z_][a-zA-Z0-9_]*'*
-- <string> ::= .*

-- Some import examples:
-- import stl.bits;
--  - imports "stl/bits.human" as "bits" (the name of the module)
-- import stl.bits as { all };
--  - imports "stl/bits.human" but only all is usable, and it doesn't need to be qualified
-- import stl.bits as epic_bits;
--  - imports "stl/bits.human" but renames it to epic_bits. All definitions must be qualified.
-- import stl.bits as epic_bits { all as bits_all, any };
--  - imports "stl/bits.human" qualified as epic_bits. Also add all, renamed to bits_all to the scope, as well as any.

data HKeyword
  = KwDef
  | KwOut
  | KwModule
  | KwImport
  | KwAs
  deriving (Eq, Show)

data HToken
  = LBracket
  | RBracket
  | LCurly
  | RCurly
  | LParen
  | RParen
  | Comma
  | Dot
  | Equals
  | DoubleColon
  | Semicolon
  | Backslash
  | Hash
  | Keyword HKeyword
  | IdStr String -- Ids (<name>)
  | Text String -- <string> (as of right now only used in the context of )
  | EOF
  deriving (Eq, Show)

singleCharTokens :: [(Char, HToken)]
singleCharTokens =
  [('[', LBracket), (']', RBracket), ('{', LCurly), ('}', RCurly), ('(', LParen), (')', RParen), (',', Comma), ('.', Dot), ('=', Equals), (';', Semicolon), ('\\', Backslash), ('#', Hash)]

escapeChar :: Char -> Maybe Char
escapeChar ch = do
  case ch of
    '"' -> return '"'
    'n' -> return '\n'
    't' -> return '\t'

seekNewline :: String -> String
seekNewline "" = ""
seekNewline ('\n' : rest) = rest
seekNewline (_ : rest) = seekNewline rest

getIdKw :: String -> Maybe (String, String)
getIdKw (ch : rest)
  | isAlpha ch || ch == '_' =
      let getMiddle (ch : rest) acc | isAlphaNum ch || ch == '_' = getMiddle rest (ch : acc)
          getMiddle rest acc = (reverse acc, rest)
          getTerminal ('\'' : rest) acc = getTerminal rest ('\'' : acc)
          getTerminal rest acc = (reverse acc, rest)
          (middle, rest') = getMiddle rest ""
          (terminal, rest'') = getTerminal rest' ""
          res = ch : middle ++ terminal
       in case res of
            "" -> Nothing
            res -> Just (res, rest'')
getIdKw _ = Nothing

convertIdKw :: String -> HToken
convertIdKw str = case str of
  "def" -> Keyword KwDef
  "out" -> Keyword KwOut
  "module" -> Keyword KwModule
  "import" -> Keyword KwImport
  "as" -> Keyword KwAs
  str -> IdStr str

data GetTokenError = UnexpectedChar Char | UnexpectedNewline | UnexpectedEOF deriving (Show)

getToken :: String -> Either GetTokenError (HToken, String)
getToken ('-' : '-' : rest) = getToken $ seekNewline rest
getToken (ch : rest) | isSpace ch = getToken rest
getToken (':' : ':' : rest) = Right (DoubleColon, rest)
getToken (ch : rest) | Just tok <- lookup ch singleCharTokens = Right (tok, rest)
getToken ('"' : rest) =
  let getStrRev ('"' : rest) acc = Right (acc, rest)
      getStrRev ('\n' : _) acc = Left UnexpectedNewline
      getStrRev ('\\' : ch : rest) acc = case escapeChar ch of
        Just ch' -> getStrRev rest (ch' : acc)
        Nothing -> Left $ UnexpectedChar ch
      getStrRev (ch : rest) acc = getStrRev rest (ch : acc)
      getStrRev "" _ = Left UnexpectedEOF
      res = getStrRev rest ""
   in second (first $ Text . reverse) res
getToken str | Just (idkw, rest) <- getIdKw str = Right (convertIdKw idkw, rest)
getToken (ch : _) = Left $ UnexpectedChar ch
getToken "" = Right (EOF, "")

tokenize :: String -> Either GetTokenError [HToken]
tokenize str = do
  (tok, str') <- getToken str
  case tok of
    EOF -> return [EOF]
    tok -> do
      rest <- tokenize str'
      return $ tok : rest