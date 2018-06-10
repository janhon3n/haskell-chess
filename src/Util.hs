module Util
where

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates =
      foldl (\seen x -> if x `elem` seen
            then seen
            else seen ++ [x]) []