module Yacht (yacht, Category(..)) where

import Data.List (sort, group, sum)

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
  let
    sortedDice = sort dice
    groupedDice = group sortedDice
    sumAllDice = sum dice

    -- Helper to count occurrences of a specific number
    countNum n = length . filter (== n)

    -- Helper to check for N-of-a-kind and return the value of the kind.
    -- Returns Nothing if not found, Just value otherwise.
    findNOfAKind n =
      let
        -- Find groups that have at least 'n' elements
        matchingGroups = filter (\g -> length g >= n) groupedDice
      in
        -- If such a group exists, return the value of its elements
        if null matchingGroups
          then Nothing
          else Just (head . head $ matchingGroups)

    -- Helper to check for specific straight
    isStraight expected = sortedDice == expected

    -- Helper to check for Yacht (5 of a kind)
    isYacht' = length groupedDice == 1 && length (head groupedDice) == 5

    -- Helper to check for Full House
    -- A full house has two groups of dice, one of length 2 and one of length 3.
    isFullHouse' = (sort . map length $ groupedDice) == [2,3]

  in
  case category of
    Ones         -> 1 * countNum 1 dice
    Twos         -> 2 * countNum 2 dice
    Threes       -> 3 * countNum 3 dice
    Fours        -> 4 * countNum 4 dice
    Fives        -> 5 * countNum 5 dice
    Sixes        -> 6 * countNum 6 dice
    FullHouse    -> if isFullHouse' then sumAllDice else 0
    FourOfAKind  ->
      case findNOfAKind 4 of
        Just val -> 4 * val
        Nothing  -> 0
    LittleStraight -> if isStraight [1,2,3,4,5] then 30 else 0
    BigStraight    -> if isStraight [2,3,4,5,6] then 30 else 0
    Choice         -> sumAllDice
    Yacht          -> if isYacht' then 50 else 0
