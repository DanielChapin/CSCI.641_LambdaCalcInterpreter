module Execution where

execute :: (a -> Maybe a) -> a -> [a]
execute step exp
  | Just exp' <- step exp = exp : execute step exp'
  | otherwise = [exp]