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
yacht category dice = case category of
  Ones           -> scoreFace 1
  Twos           -> scoreFace 2
  Threes         -> scoreFace 3
  Fours          -> scoreFace 4
  Fives          -> scoreFace 5
  Sixes          -> scoreFace 6
  FullHouse      -> if lengths == [2, 3] then total else 0
  FourOfAKind    ->
    case filter ((>= 4) . length) gs of
      (g:_) -> 4 * head g
      _     -> 0
  LittleStraight -> if sorted == [1,2,3,4,5] then 30 else 0
  BigStraight    -> if sorted == [2,3,4,5,6] then 30 else 0
  Choice         -> total
  Yacht          -> if any ((== 5) . length) gs then 50 else 0
  where
    sorted   = sort dice
    gs       = group sorted
    lengths  = sort (map length gs)
    total    = sum dice
    scoreFace n = n * length (filter (== n) dice)
