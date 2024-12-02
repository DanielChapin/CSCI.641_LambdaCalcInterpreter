module Peano where

data Peano = Succ Peano | Zero

intToPeano :: Int -> Peano
intToPeano 0 = Zero
intToPeano n = Succ $ intToPeano (n - 1)

peanoToInt :: Peano -> Int
peanoToInt Zero = 0
peanoToInt (Succ n) = 1 + peanoToInt n
