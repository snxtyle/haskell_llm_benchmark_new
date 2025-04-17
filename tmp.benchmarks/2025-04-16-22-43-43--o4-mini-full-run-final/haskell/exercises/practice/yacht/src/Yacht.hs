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
              deriving (Eq, Show)

yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones            -> sumOf 1
    Twos            -> sumOf 2
    Threes          -> sumOf 3
    Fours           -> sumOf 4
    Fives           -> sumOf 5
    Sixes           -> sumOf 6
    FullHouse       -> if isFullHouse then total else 0
    FourOfAKind     -> fourOfAKindScore
    LittleStraight  -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight     -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice          -> total
    Yacht           -> if length grouped == 1 then 50 else 0
  where
    sorted    = sort dice
    grouped   = group sorted
    total     = sum dice
    sumOf n   = n * length (filter (== n) dice)
    isFullHouse = case map length grouped of
                    [2,3] -> True
                    [3,2] -> True
                    _     -> False
    fourOfAKindScore =
      case filter ((>=4) . length) grouped of
        (g:_) -> head g * 4
        []    -> 0
