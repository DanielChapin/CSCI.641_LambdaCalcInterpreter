module Consts where

import HumanParser (HExp (..))

charLambda :: Char
charLambda = 'λ'

humanYCombinator :: HExp
humanYCombinator = Lambda "x" $ Apply (Id Nothing "x") (Id Nothing "x")