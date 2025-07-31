module RailFenceCipher (encode, decode) where

import Data.List (sortBy)

encode :: Int -> String -> String
encode rails message
  | rails <= 1 = message
  | otherwise = concatMap (map snd) $ groupByRail $ zip (railIndices rails (length message)) message
  where
    groupByRail = foldr (\(r, c) acc -> insertAt r c acc) (replicate rails [])
    insertAt 0 x (xs:xss) = (x:xs) : xss
    insertAt n x (xs:xss) = xs : insertAt (n-1) x xss
    insertAt _ _ [] = []

decode :: Int -> String -> String
decode rails cipher
  | rails <= 1 = cipher
  | otherwise = map snd $ sortBy compareIndex $ zip (railIndices rails (length cipher)) cipher
  where
    compareIndex (i1, _) (i2, _) = compare i1 i2

railIndices :: Int -> Int -> [Int]
railIndices rails len = take len $ cycle $ [0..rails-2] ++ [rails-1, rails-2..1]
