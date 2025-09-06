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
yacht Ones dice = sum [x | x <- dice, x == 1]
yacht Twos dice = sum [x | x <- dice, x == 2]
yacht Threes dice = sum [x | x <- dice, x == 3]
yacht Fours dice = sum [x | x <- dice, x == 4]
yacht Fives dice = sum [x | x <- dice, x == 5]
yacht Sixes dice = sum [x | x <- dice, x == 6]
yacht FullHouse dice = 
    let sorted = sort dice
        grouped = group sorted
        counts = map length grouped
    in if counts == [2,3] || counts == [3,2]
       then sum dice
       else 0
yacht FourOfAKind dice =
    let sorted = sort dice
        grouped = group sorted
        counts = map length grouped
        values = map head grouped
    in case findWithCount 4 grouped of
        Just val -> 4 * val
        Nothing -> if length dice == 5 && (head counts == 5)
                   then 4 * (head dice)
                   else 0
  where findWithCount n groups = case filter (\g -> length g >= n) groups of
                                   [] -> Nothing
                                   (g:_) -> Just (head g)
yacht LittleStraight dice = 
    if sort dice == [1,2,3,4,5] then 30 else 0
yacht BigStraight dice = 
    if sort dice == [2,3,4,5,6] then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice =
    if length (group (sort dice)) == 1 then 50 else 0
