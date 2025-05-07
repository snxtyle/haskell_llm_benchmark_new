module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ topLabel : map rowString [0..7] ++ [topLabel]
  where
    topLabel = "  a b c d e f g h"
    rowString r = show (8 - r) ++ " " ++ unwords [square r c | c <- [0..7]] ++ " " ++ show (8 - r)
    square r c
      | Just (r, c) == white = "W"
      | Just (r, c) == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
  r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)
