module UTLC where

import Data.List

data LExp var
  = Abstraction var (LExp var)
  | Application (LExp var) (LExp var)
  | Var var

instance {-# OVERLAPPING #-} Show (LExp String) where
  show (Abstraction param body) = "(\\" ++ param ++ ". " ++ show body ++ ")"
  show (Application lhs rhs) = "(" ++ show lhs ++ " " ++ show rhs ++ ")"
  show (Var var) = var

instance Show a => Show (LExp a) where
  show (Abstraction param body) = "(\\" ++ show param ++ ". " ++ show body ++ ")"
  show (Application lhs rhs) = "(" ++ show lhs ++ " " ++ show rhs ++ ")"
  show (Var var) = show var

betaReduce :: Eq a => a -> LExp a -> LExp a -> LExp a
betaReduce x (Abstraction v body) b = Abstraction v (betaReduce x body b)
betaReduce x (Application lhs rhs) b = Application (betaReduce x lhs b) (betaReduce x rhs b)
betaReduce x e@(Var y) b
  | x == y = b
  | otherwise = e

-- alphaConvert from to exp
alphaConvert :: Eq a => a -> a -> LExp a -> LExp a
alphaConvert f t (Abstraction param body)
  | param == f = Abstraction t (alphaConvert f t body)
  | otherwise = Abstraction param (alphaConvert f t body)
alphaConvert f t (Application l r) = Application (alphaConvert f t l) (alphaConvert f t r)
alphaConvert f t (Var v)
  | v == f = Var t
  | otherwise = Var v

boundVars :: Eq a => LExp a -> [a]
boundVars exp =
  let boundVars' (Abstraction v body) = v : boundVars' body
      boundVars' (Application l r) = boundVars' l ++ boundVars' r
      boundVars' _ = []
   in nub $ boundVars' exp

makeFreshVar :: Eq a => [a] -> a -> (a -> a) -> a
makeFreshVar used v next
  | v `notElem` used = v
  | otherwise = makeFreshVar used (next v) next

alphaAll :: Eq a => [a] -> (a -> a) -> LExp a -> LExp a
-- Finish condition: none of the bound variables in exp are in the list of used variables.
alphaAll used next exp =
  let bound = boundVars exp
   in case used `intersect` bound of
        [] -> exp
        xs ->
          fst $
            foldr
              (\x (exp, bound) -> let x' = makeFreshVar bound x next in (alphaConvert x x' exp, x' : bound))
              (exp, filter (`notElem` xs) used ++ bound)
              xs

resolveAlpha :: Eq a => (a -> a) -> LExp a -> LExp a -> (LExp a, LExp a)
resolveAlpha next l@(Abstraction x b) r = (l, alphaAll (filter (/= x) $ boundVars l) next r)
resolveAlpha next l r = (l, alphaAll (boundVars l) next r)

step :: Eq a => (a -> a) -> LExp a -> Maybe (LExp a)
step next (Application l r)
  | Just l' <- step next l = Just $ Application l' r
  | Just r' <- step next r = Just $ Application l r'
  -- Consider the following expression: ((\y. \x. y x) (\x. x))
  -- This evaluates to (\x. (\x. x) x), which now has ambiguity (the x in the body of the inner abstraction).
  -- As such, whenever we are performing a beta reduction, we must first check if there is any overlap in variable names.
  -- To resolve this, we need to swap to a free variable (alpha-conversion)
  -- Also consider ((\x. \x'. x) (\x. \x''. x))...
  -- If we're creating new variables by appending ' then we can run into a collision within the reducing expression.
  -- TODO Maybe the batch of alpha reductions should be their own step?
  | (Abstraction v body, r) <- resolveAlpha next l r = Just $ betaReduce v body r
step _ _ = Nothing

stepHistory :: Eq a => (a -> a) -> LExp a -> [LExp a]
stepHistory next e =
  let stepHistory' e
        | Just e' <- step next e = e' : stepHistory' e'
        | otherwise = []
   in e : stepHistory' e