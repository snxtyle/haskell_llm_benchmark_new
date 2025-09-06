module Yacht (yacht, Category(..)) where

import Data.List (sort, group, find)

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

countValue :: Int -> [Int] -> Int
countValue value dice = sum $ filter (== value) dice

isFullHouse :: [Int] -> Bool
isFullHouse dice =
  let sorted = sort dice
      counts = map length $ group sorted
  in sort counts == [2, 3]

getFourOfAKindScore :: [Int] -> Int
getFourOfAKindScore dice =
  let sorted = sort dice
      groups = group sorted
      fourGroup = find ((>= 4) . length) groups
  in case fourGroup of
       Just xs -> 4 * head xs
       Nothing -> 0

isLittleStraight :: [Int] -> Bool
isLittleStraight dice = sort dice == [1, 2, 3, 4, 5]

isBigStraight :: [Int] -> Bool
isBigStraight dice = sort dice == [2, 3, 4, 5, 6]

isYacht :: [Int] -> Bool
isYacht dice = all (== head dice) dice

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
  Ones -> countValue 1 dice
  Twos -> countValue 2 dice
  Threes -> countValue 3 dice
  Fours -> countValue 4 dice
  Fives -> countValue 5 dice
  Sixes -> countValue 6 dice
  FullHouse -> if isFullHouse dice then sum dice else 0
  FourOfAKind -> getFourOfAKindScore dice
  LittleStraight -> if isLittleStraight dice then 30 else 0
  BigStraight -> if isBigStraight dice then 30 else 0
  Choice -> sum dice
  Yacht -> if isYacht dice then 50 else 0
