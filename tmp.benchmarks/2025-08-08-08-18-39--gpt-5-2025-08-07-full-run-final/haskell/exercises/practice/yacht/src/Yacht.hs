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

yacht :: Category -> [Int] -> Int
yacht category dice =
  let sd = sort dice
      gs = group sd
      lens = map length gs
      sumDice = sum dice
      countOf n = length (filter (== n) dice)
  in case category of
      Ones           -> 1 * countOf 1
      Twos           -> 2 * countOf 2
      Threes         -> 3 * countOf 3
      Fours          -> 4 * countOf 4
      Fives          -> 5 * countOf 5
      Sixes          -> 6 * countOf 6
      FullHouse      ->
        if sort lens == [2, 3] then sumDice else 0
      FourOfAKind    ->
        case find (\g -> length g >= 4) gs of
          Just g  -> 4 * head g
          Nothing -> 0
      LittleStraight ->
        if sd == [1,2,3,4,5] then 30 else 0
      BigStraight    ->
        if sd == [2,3,4,5,6] then 30 else 0
      Choice         -> sumDice
      Yacht          ->
        if any ((== 5) . length) gs then 50 else 0
