module Yacht (yacht, Category(..)) where

import Data.List (group, sort)

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

-- | Calculate the score obtained by rolling five dice for a given category.
--   If the dice do not match the requirements of the category, 0 is returned.
yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones           -> faceScore 1
    Twos           -> faceScore 2
    Threes         -> faceScore 3
    Fours          -> faceScore 4
    Fives          -> faceScore 5
    Sixes          -> faceScore 6
    FullHouse      -> if isFullHouse then total else 0
    FourOfAKind    -> maybe 0 (\v -> v * 4) fourKindValue
    LittleStraight -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight    -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice         -> total
    Yacht          -> if isYacht then 50 else 0
  where
    total = sum dice

    faceScore n = n * length (filter (== n) dice)

    -- Frequency list: [(face value, occurrences)]
    freq = map (\xs@(x:_) -> (x, length xs)) . group . sort $ dice

    counts = map snd freq

    isYacht = any (== 5) counts

    isFullHouse = sort counts == [2,3]

    fourKindValue =
      case filter ((>= 4) . snd) freq of
        (v, _):_ -> Just v
        []       -> Nothing
