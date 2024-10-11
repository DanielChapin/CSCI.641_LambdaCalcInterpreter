module AST where

data HToken = LCurly | RCurly 
  | LBracket | RBracket 
  | LParen | RParen
  | Star
  | Comma
  | IdKw String -- Ids and Keywords
  | IntLiteral Integer -- ints
  | StrLiteral String -- strings
  | BoolLiteral Bool -- booleans

data HExp = Import String -- import "test.human" as Test::{hello_world, test_func} in
  | List [HExp] -- lists
  | If HExp HExp HExp
