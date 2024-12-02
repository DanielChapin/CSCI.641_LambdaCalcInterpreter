module Execution where

execute :: (a -> Maybe a) -> a -> [a]
execute step exp
  | Just exp' <- step exp = exp : execute step exp'
  | otherwise = [exp]

executeResult :: (a -> Maybe a) -> a -> a
executeResult step exp
  | Just exp' <- step exp = executeResult step exp'
  | otherwise = exp