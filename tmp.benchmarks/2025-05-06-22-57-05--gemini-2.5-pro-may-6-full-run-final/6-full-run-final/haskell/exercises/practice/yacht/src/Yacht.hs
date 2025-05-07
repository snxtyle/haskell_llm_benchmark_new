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

-- Helper function to count occurrences of each die value.
-- It sorts the dice, groups identical adjacent elements, then maps each group
-- to a tuple of (die_value, count).
-- Example: counts [1,1,2,3,3] might return [(1,2),(2,1),(3,2)] (order of tuples depends on initial sort)
counts :: [Int] -> [(Int, Int)]
counts dice = map (\xs -> (head xs, length xs)) . group . sort $ dice

-- Helper function to score categories Ones, Twos, Threes, Fours, Fives, Sixes.
-- It filters the dice for the specific value, counts them, and multiplies by the value.
scoreSingle :: Int -> [Int] -> Int
scoreSingle val dice = val * (length $ filter (== val) dice)

yacht :: Category -> [Int] -> Int
yacht Ones           dice = scoreSingle 1 dice
yacht Twos           dice = scoreSingle 2 dice
yacht Threes         dice = scoreSingle 3 dice
yacht Fours          dice = scoreSingle 4 dice
yacht Fives          dice = scoreSingle 5 dice
yacht Sixes          dice = scoreSingle 6 dice

yacht FullHouse      dice =
    let cs = counts dice
        -- A full house has three of one kind and two of another.
        -- This means there are exactly two distinct die values.
        -- One value must have a count of 3, and the other a count of 2.
        isThreeKind = any (\(_, count) -> count == 3) cs
        isTwoKind   = any (\(_, count) -> count == 2) cs
    in if length cs == 2 && isThreeKind && isTwoKind then sum dice else 0

yacht FourOfAKind    dice =
    let cs = counts dice
        -- Find if any die value appears at least 4 times.
        -- The result of filter will be a list; we are interested if it's non-empty.
        -- If it is, take the die value (val) from the first such group.
        fourOfKindDieValue = case filter (\(_, count) -> count >= 4) cs of
                                ((val, _):_) -> Just val -- Found a die with count >= 4
                                []           -> Nothing  -- No die with count >= 4
    in case fourOfKindDieValue of
        Just val -> val * 4 -- Score is the sum of those four dice
        Nothing  -> 0

yacht LittleStraight dice =
    -- A little straight is 1-2-3-4-5.
    if sort dice == [1,2,3,4,5] then 30 else 0

yacht BigStraight    dice =
    -- A big straight is 2-3-4-5-6.
    if sort dice == [2,3,4,5,6] then 30 else 0

yacht Choice         dice =
    -- Choice is the sum of all dice.
    sum dice

yacht Yacht          dice =
    let cs = counts dice
        -- A Yacht is all five dice showing the same face.
        -- This means one die value appears 5 times.
    in if any (\(_, count) -> count == 5) cs then 50 else 0
