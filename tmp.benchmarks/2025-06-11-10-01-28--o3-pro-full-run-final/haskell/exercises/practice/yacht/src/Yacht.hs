module Yacht (yacht, Category(..)) where

import Data.List (group, sort)

-- | All the possible scoring categories in the game of Yacht.
--   We derive 'Eq' and 'Show' so the values can be compared and
--   displayed in the tests.
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
              deriving (Eq, Show)

-- | Calculate the score for a given set of five dice and a category.
--
--   The function follows the official Yacht rules:
--   • Ones-Sixes      – n × (count of n’s)
--   • Full House      – three of one value and two of another, scores the sum
--   • Four Of A Kind  – at least four dice the same, scores 4 × that value
--   • Little Straight – 1-2-3-4-5 exactly, scores 30
--   • Big Straight    – 2-3-4-5-6 exactly, scores 30
--   • Choice          – sum of all dice
--   • Yacht           – all five dice equal, scores 50
--   Any category not satisfied scores 0.
yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones            -> singles 1
    Twos            -> singles 2
    Threes          -> singles 3
    Fours           -> singles 4
    Fives           -> singles 5
    Sixes           -> singles 6
    FullHouse       -> fullHouse
    FourOfAKind     -> fourOfAKind
    LittleStraight  -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight     -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice          -> sum dice
    Yacht           -> if isYacht then 50 else 0
  where
    -- count how many of the given face value appear, multiply by the face value
    singles n = n * length (filter (== n) dice)

    sortedGroups = group (sort dice) -- groups of equal dice, already sorted
    frequencies  = map length sortedGroups

    isYacht = frequencies == [5]

    fullHouse
      | sort frequencies == [2,3] = sum dice
      | otherwise                 = 0

    fourOfAKind =
      case filter (\g -> length g >= 4) sortedGroups of
        (g:_) -> 4 * head g
        _     -> 0
