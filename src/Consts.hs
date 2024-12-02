module Consts where

import HumanParser (HExp (..))

charLambda :: Char
charLambda = 'λ'

humanYCombinator :: HExp
humanYCombinator =
  let f' = "fun*"
      f = Id Nothing f'
      x' = "arg*"
      x = Id Nothing x'
   in Lambda f' (Apply (Lambda x' $ Apply x x) (Lambda x' $ Apply f (Apply x x)))

humanTrue :: HExp
humanTrue = Lambda "x" $ Lambda "y" $ Id Nothing "x"

humanFalse :: HExp
humanFalse = Lambda "x" $ Lambda "y" $ Id Nothing "y"

humanPair :: HExp -> HExp -> HExp
humanPair first second = Lambda "oper" (Apply (Apply (Id Nothing "oper") first) second)

humanId :: HExp
humanId = Lambda "x" $ Id Nothing "x"