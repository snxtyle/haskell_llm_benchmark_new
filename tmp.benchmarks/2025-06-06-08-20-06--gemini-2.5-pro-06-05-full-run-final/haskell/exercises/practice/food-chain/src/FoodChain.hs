module FoodChain (song) where

import Data.List (intercalate)

-- A tuple to hold information about each animal.
-- (name, remark for its verse, suffix for when it's being caught)
type Animal = (String, String, String)

animals :: [Animal]
animals =
    [ ("fly",    "I don't know why she swallowed the fly. Perhaps she'll die.", "")
    , ("spider", "It wriggled and jiggled and tickled inside her.", " that wriggled and jiggled and tickled inside her")
    , ("bird",   "How absurd to swallow a bird!", "")
    , ("cat",    "Imagine that, to swallow a cat!", "")
    , ("dog",    "What a hog, to swallow a dog!", "")
    , ("goat",   "Just opened her throat and swallowed a goat!", "")
    , ("cow",    "I don't know how she swallowed a cow!", "")
    , ("horse",  "She's dead, of course!", "")
    ]

-- Helper functions to access parts of the Animal tuple for clarity.
name :: Animal -> String
name (n, _, _) = n

remark :: Animal -> String
remark (_, r, _) = r

catchSuffix :: Animal -> String
catchSuffix (_, _, s) = s

-- Generates the entire song by creating each verse and joining them.
song :: String
song = (intercalate "\n\n" $ map verse [0..length animals - 1]) ++ "\n"

-- Generates a single verse based on its index.
verse :: Int -> String
verse n
    -- The first and last verses are simple and non-cumulative.
    | n == 0 || n == length animals - 1 = simpleVerse n
    -- All other verses are cumulative.
    | otherwise                         = cumulativeVerse n

-- Generates a simple, non-cumulative verse (for fly and horse).
simpleVerse :: Int -> String
simpleVerse n =
    let animal = animals !! n
    in intercalate "\n" [ "I know an old lady who swallowed a " ++ name animal ++ "."
                        , remark animal
                        ]

-- Generates a cumulative verse.
cumulativeVerse :: Int -> String
cumulativeVerse n =
    let animal = animals !! n
        intro = "I know an old lady who swallowed a " ++ name animal ++ "."
        -- The chain of "swallowed to catch" lines.
        chain = map chainLine [n, n-1 .. 1]
        -- The common ending for cumulative verses.
        ending = remark (head animals)
    in intercalate "\n" $ [intro, remark animal] ++ chain ++ [ending]

-- Generates a "She swallowed the X to catch the Y" line.
chainLine :: Int -> String
chainLine i =
    let swallower = animals !! i
        prey      = animals !! (i-1)
    in "She swallowed the " ++ name swallower ++ " to catch the " ++ name prey ++ catchSuffix prey ++ "."
