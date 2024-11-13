module AST where

-- Program ::= module <name>(.<name>)*; Statement*
--         ::= Statement*
--
-- Statement ::= ImportStatement
--           ::= -- <string>
--           ::= Def | Out
--
-- ImportStatement ::= import <name>;
--                 ::= import <name> as Import(, Import)*;
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
--     ::= #<name>[<string>] | #<name>![Exp]
--
-- <name> ::= [a-zA-Z_][a-zA-Z0-9_]*'*
-- <string> ::= .*

data HToken
  = LBracket
  | RBracket
  | LCurly
  | RCurly
  | LParen
  | RParen
  | Comma
  | Colon
  | DoubleColon
  | Backslash
  | Hash
  | IdKw String -- Ids and Keywords (<name>)
  | Text String

data HExp = Encode String String

data HStatement
  = Import String (Maybe String) [String] -- import "test.human" as Test::{hello_world, test_func} in
  | Def String HExp
  | Evl String String HExp
