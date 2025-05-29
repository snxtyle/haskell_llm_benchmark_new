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

yacht :: Category -> [Int] -> Int
yacht Ones dice = 1 * length (filter (==1) dice)
yacht Twos dice = 2 * length (filter (==2) dice)
yacht Threes dice = 3 * length (filter (==3) dice)
yacht Fours dice = 4 * length (filter (==4) dice)
yacht Fives dice = 5 * length (filter (==5) dice)
yacht Sixes dice = 6 * length (filter (==6) dice)
yacht FullHouse dice = 
          if counts == [2,3] then sum dice else 0
          where groups = map length $ group $ sort dice
                counts = sort groups
yacht FourOfAKind dice = 
          case [ head g | g <- group (sort dice), length g >=4 ] of
              (x:_) -> 4 * x
              _      -> 0
yacht LittleStraight dice = if sort dice == [1,2,3,4,5] then 30 else 0
yacht BigStraight dice    = if sort dice == [2,3,4,5,6] then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice = 
          case group (sort dice) of
              [x] -> 50
              _   -> 0
