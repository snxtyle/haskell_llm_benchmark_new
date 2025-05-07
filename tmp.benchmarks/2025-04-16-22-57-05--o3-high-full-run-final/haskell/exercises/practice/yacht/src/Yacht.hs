module Yacht (yacht, Category(..)) where

import Data.List  (group, sort)
import Data.Maybe (listToMaybe)

data Category
    = Ones
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
        Ones           -> faceScore 1
        Twos           -> faceScore 2
        Threes         -> faceScore 3
        Fours          -> faceScore 4
        Fives          -> faceScore 5
        Sixes          -> faceScore 6
        FullHouse      -> if isFullHouse grouped then sum dice else 0
        FourOfAKind    -> maybe 0 (* 4) fourKindValue
        LittleStraight -> if sort dice == [1, 2, 3, 4, 5] then 30 else 0
        BigStraight    -> if sort dice == [2, 3, 4, 5, 6] then 30 else 0
        Choice         -> sum dice
        Yacht          -> if length grouped == 1 then 50 else 0
  where
    grouped :: [[Int]]
    grouped = group (sort dice)

    -- Score for the simple "n times face value" categories
    faceScore :: Int -> Int
    faceScore n = n * length (filter (== n) dice)

    -- Full house requires exactly two distinct faces, one appearing three times
    -- and the other appearing twice.
    isFullHouse :: [[a]] -> Bool
    isFullHouse g = length g == 2 && sort (map length g) == [2, 3]

    -- Find a value that appears at least four times (if any).
    fourKindValue :: Maybe Int
    fourKindValue =
        (\grp -> head grp) <$> listToMaybe (filter ((>= 4) . length) grouped)
