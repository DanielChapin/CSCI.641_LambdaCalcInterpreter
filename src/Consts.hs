module Consts where

import HumanParser (HExp (..))

charLambda :: Char
charLambda = 'λ'

humanYCombinator :: HExp
humanYCombinator = Lambda "x" $ Apply (Id Nothing "x") (Id Nothing "x")

humanTrue :: HExp
humanTrue = Lambda "x" $ Lambda "y" $ Id Nothing "x"

humanFalse :: HExp
humanFalse = Lambda "x" $ Lambda "y" $ Id Nothing "y"

humanPair :: HExp -> HExp -> HExp
humanPair first second = Lambda "oper" (Apply (Apply (Id Nothing "oper") first) second)

humanId :: HExp
humanId = Lambda "x" $ Id Nothing "x"