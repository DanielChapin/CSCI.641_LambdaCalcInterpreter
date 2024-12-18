module Helpers where

import Data.Either (partitionEithers)
import Data.Set (Set, elems, empty, insert, member)

allDuplicates :: Ord a => [a] -> [a]
allDuplicates vals = allDuplicates' vals empty empty

allDuplicates' :: Ord a => [a] -> Set a -> Set a -> [a]
allDuplicates' [] _ acc = elems acc
allDuplicates' (val : rest) used acc
  | member val used = allDuplicates' rest used (insert val acc)
  | otherwise = allDuplicates' rest (insert val used) acc

selectErrorsElseValues :: [Either l r] -> Either [l] [r]
selectErrorsElseValues eithers =
  let (lefts, rights) = partitionEithers eithers
   in case lefts of
        [] -> Right rights
        lefts -> Left lefts