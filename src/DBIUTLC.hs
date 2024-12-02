module DBIUTLC where

import Consts (charLambda)
import Data.Text (replace)

data DBILExp
  = Abstraction String DBILExp
  | Application DBILExp DBILExp
  | Var Integer
  deriving (Eq)

showVarLabeled :: [String] -> Integer -> String
showVarLabeled varNames index =
  let getName [] i = "???"
      getName (name : _) 0 = name
      getName (_ : names) n | n > 0 = getName names (n - 1)
      name = getName varNames index
   in name ++ "[" ++ show index ++ "]"

showContextAbs :: [String] -> DBILExp -> String
showContextAbs varNames (Abstraction name body) = "\\" ++ name ++ "." ++ showContextAbs (name : varNames) body
showContextAbs varNames (Application l r) = showContextAppLeft varNames l ++ " " ++ showContextAppRight varNames r
showContextAbs varNames (Var x) = showVarLabeled varNames x

showContextAppLeft :: [String] -> DBILExp -> String
showContextAppLeft varNames exp@(Abstraction _ _) = "(" ++ showContextAbs varNames exp ++ ")"
showContextAppLeft varNames (Application l r@(Var _)) = showContextAppLeft varNames l ++ " " ++ showContextAppRight varNames r
showContextAppLeft varNames (Application l r) = "(" ++ showContextAppLeft varNames l ++ " " ++ showContextAppRight varNames r ++ ")"
showContextAppLeft varNames (Var x) = showVarLabeled varNames x

showContextAppRight :: [String] -> DBILExp -> String
showContextAppRight varNames exp@(Abstraction _ _) = showContextAbs varNames exp
showContextAppRight varNames (Application l r) = "(" ++ showContextAppLeft varNames l ++ " " ++ showContextAbs varNames r ++ ")"
showContextAppRight varNames (Var x) = showVarLabeled varNames x

instance Show DBILExp where
  show = showContextAbs []

shift :: Integer -> Integer -> DBILExp -> DBILExp
shift d c v@(Var x)
  | x < c = v
  | otherwise = Var (x + d)
shift d c (Abstraction name body) = Abstraction name (shift d (c + 1) body)
shift d c (Application l r) = Application (shift d c l) (shift d c r)

substitute :: Integer -> DBILExp -> DBILExp -> DBILExp
substitute target substitution v@(Var x)
  | x == target = substitution
  | otherwise = v
substitute target substitution (Abstraction name body) =
  Abstraction name (substitute (target + 1) (shift 1 0 substitution) body)
substitute target substitution (Application l r) =
  Application (substitute target substitution l) (substitute target substitution r)

step :: DBILExp -> Maybe DBILExp
step (Application l r) | Just l' <- step l = Just $ Application l' r
step (Application (Abstraction name body) r) = Just $ substitute 0 r body
step (Application l r) | Just r' <- step r = Just $ Application l r'
step _ = Nothing

stepHistory :: DBILExp -> [DBILExp]
stepHistory e =
  let stepHistory' e
        | Just e' <- step e = e' : stepHistory' e'
        | otherwise = []
   in e : stepHistory' e