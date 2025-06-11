module Yacht (yacht, Category(..)) where

import Data.List (find, group, sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht Ones           = countSingle 1
yacht Twos           = countSingle 2
yacht Threes         = countSingle 3
yacht Fours          = countSingle 4
yacht Fives          = countSingle 5
yacht Sixes          = countSingle 6
yacht FullHouse      = scoreFullHouse
yacht FourOfAKind    = scoreFourOfAKind
yacht LittleStraight = scoreStraight [1, 2, 3, 4, 5]
yacht BigStraight    = scoreStraight [2, 3, 4, 5, 6]
yacht Choice         = sum
yacht Yacht          = scoreYacht

-- | Scores for categories Ones to Sixes.
-- It sums the dice that match the given number.
countSingle :: Int -> [Int] -> Int
countSingle n = sum . filter (== n)

-- | Helper to get frequency of each die value.
-- e.g. [1,1,2,5,5] -> [(1,2), (2,1), (5,2)]
counts :: [Int] -> [(Int, Int)]
counts = map (\g -> (head g, length g)) . group . sort

-- | Score for a full house (3 of one, 2 of another).
scoreFullHouse :: [Int] -> Int
scoreFullHouse dice =
  let cs = map snd $ counts dice
   in if sort cs == [2, 3] then sum dice else 0

-- | Score for four of a kind.
scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind dice =
  case find ((>= 4) . snd) (counts dice) of
    Just (face, _) -> face * 4
    Nothing -> 0

-- | Score for a straight.
scoreStraight :: [Int] -> [Int] -> Int
scoreStraight target dice =
  if sort dice == target then 30 else 0

-- | Score for a yacht (5 of a kind).
scoreYacht :: [Int] -> Int
scoreYacht dice =
  if length (counts dice) == 1 then 50 else 0
