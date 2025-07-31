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
yacht category dice =
  case category of
    Ones           -> faceSum 1
    Twos           -> faceSum 2
    Threes         -> faceSum 3
    Fours          -> faceSum 4
    Fives          -> faceSum 5
    Sixes          -> faceSum 6
    FullHouse      -> if isFullHouse counts then sum dice else 0
    FourOfAKind    -> case fourOfAKindValue counts of
                        Just v  -> 4 * v
                        Nothing -> 0
    LittleStraight -> if sort dice == [1,2,3,4,5] then 30 else 0
    BigStraight    -> if sort dice == [2,3,4,5,6] then 30 else 0
    Choice         -> sum dice
    Yacht          -> if isYacht counts then 50 else 0
  where
    faceSum n = n * length (filter (== n) dice)
    counts = countFaces dice

-- Count occurrences of each die face (1..6) and pair with value.
-- Returns list of (faceValue, count) for faces that appear at least once.
countFaces :: [Int] -> [(Int, Int)]
countFaces ds =
  let grouped = group (sort ds) -- groups of identical values
  in map (\g -> (head g, length g)) grouped

isYacht :: [(Int, Int)] -> Bool
isYacht cs = any ((== 5) . snd) cs

isFullHouse :: [(Int, Int)] -> Bool
isFullHouse cs =
  -- Full house must be exactly two distinct faces with counts 3 and 2.
  case sort (map snd cs) of
    [2,3] -> True
    _     -> False

fourOfAKindValue :: [(Int, Int)] -> Maybe Int
fourOfAKindValue cs =
  -- At least four of a kind qualifies, including 5 of a kind.
  case filter (\(_, c) -> c >= 4) cs of
    (v, _):_ -> Just v
    []       -> Nothing
