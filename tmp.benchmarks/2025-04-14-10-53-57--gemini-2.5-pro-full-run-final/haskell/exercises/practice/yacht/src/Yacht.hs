module Yacht (yacht, Category(..)) where

import Data.List (sort, group)

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

-- Helper function to calculate score for number categories (Ones to Sixes)
scoreNumber :: Int -> [Int] -> Int
scoreNumber n dice = n * length (filter (== n) dice)

-- Helper function to get counts of each die value as a list of (value, count) pairs
-- Example: getCounts [1, 1, 2, 3, 3] == [(1, 2), (2, 1), (3, 2)]
getCounts :: [Int] -> [(Int, Int)]
getCounts = map (\g -> (head g, length g)) . group . sort

yacht :: Category -> [Int] -> Int
yacht category dice =
  let sortedDice = sort dice
      countsList = getCounts dice -- e.g., [(3, 3), (5, 2)] for [5,3,5,3,3]
      -- Extract the counts (lengths of groups) and sort them
      -- e.g., [2, 3] for [5,3,5,3,3] or [5] for [4,4,4,4,4]
      sortedCounts = sort $ map snd countsList
      totalSum = sum dice
  in case category of
    Ones           -> scoreNumber 1 dice
    Twos           -> scoreNumber 2 dice
    Threes         -> scoreNumber 3 dice
    Fours          -> scoreNumber 4 dice
    Fives          -> scoreNumber 5 dice
    Sixes          -> scoreNumber 6 dice

    FullHouse      -> -- Check if the counts are exactly one pair and one triple
                      if sortedCounts == [2, 3] then totalSum else 0

    FourOfAKind    -> -- Find if any die value appears at least 4 times
                      case filter (\(_, count) -> count >= 4) countsList of
                        -- If found, take the die value and multiply by 4
                        ((value, _):_) -> value * 4
                        -- Otherwise, score is 0
                        [] -> 0

    LittleStraight -> -- Check if the sorted dice match 1, 2, 3, 4, 5
                      if sortedDice == [1, 2, 3, 4, 5] then 30 else 0

    BigStraight    -> -- Check if the sorted dice match 2, 3, 4, 5, 6
                      if sortedDice == [2, 3, 4, 5, 6] then 30 else 0

    Choice         -> -- Score is the sum of all dice
                      totalSum

    Yacht          -> -- Check if there's only one group and its size is 5 (all dice same)
                      if sortedCounts == [5] then 50 else 0
