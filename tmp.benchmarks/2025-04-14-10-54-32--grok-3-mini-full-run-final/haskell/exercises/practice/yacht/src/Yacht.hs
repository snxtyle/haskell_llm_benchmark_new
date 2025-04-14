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

yacht :: Category -> [Int] -> Int
yacht category dice =
  let sortedDice = sort dice
      grouped = group sortedDice
      lengths = sort $ map length grouped
  in case category of
       Ones -> 1 * length (filter (== 1) dice)
       Twos -> 2 * length (filter (== 2) dice)
       Threes -> 3 * length (filter (== 3) dice)
       Fours -> 4 * length (filter (== 4) dice)
       Fives -> 5 * length (filter (== 5) dice)
       Sixes -> 6 * length (filter (== 6) dice)
       FullHouse -> if lengths == [2, 3] then sum dice else 0
       FourOfAKind -> if any (>= 4) (map length grouped)
                      then let fourKind = head [x | g <- grouped, length g >= 4, let x = head g]
                           in fourKind * 4
                      else 0
       LittleStraight -> if sortedDice == [1, 2, 3, 4, 5] then 30 else 0
       BigStraight -> if sortedDice == [2, 3, 4, 5, 6] then 30 else 0
       Choice -> sum dice
       Yacht -> if lengths == [5] then 50 else 0
