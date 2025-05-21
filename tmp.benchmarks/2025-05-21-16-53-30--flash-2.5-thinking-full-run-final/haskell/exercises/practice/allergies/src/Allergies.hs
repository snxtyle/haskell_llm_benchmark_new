module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.)) -- Import for bitwise AND

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

-- Helper list to map Allergens to their scores
allAllergensWithScores :: [(Allergen, Int)]
allAllergensWithScores =
    [ (Eggs, 1)
    , (Peanuts, 2)
    , (Shellfish, 4)
    , (Strawberries, 8)
    , (Tomatoes, 16)
    , (Chocolate, 32)
    , (Pollen, 64)
    , (Cats, 128)
    ]

-- The maximum possible score for listed allergens is 1+2+4+8+16+32+64+128 = 255.
-- Any bit higher than 128 (i.e., 256, 512, etc.) should be ignored.
-- This means we should effectively take the score modulo 256, or bitwise AND with 255.
-- For example, 257 (0b100000001) should be treated as 1 (0b000000001).
-- 257 .&. 255 = 1.
-- 256 .&. 255 = 0.
-- 255 .&. 255 = 255.
-- This is the correct interpretation of "ignore those components".
effectiveScore :: Int -> Int
effectiveScore score = score .&. 255 -- Mask out bits higher than 128

allergies :: Int -> [Allergen]
allergies score =
    let maskedScore = effectiveScore score
    in [ allergen
       | (allergen, allergenValue) <- allAllergensWithScores
       , (maskedScore .&. allergenValue) == allergenValue
       ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
    let maskedScore = effectiveScore score
        -- Find the corresponding integer value for the given allergen
        allergenValue = case allergen of
            Eggs -> 1
            Peanuts -> 2
            Shellfish -> 4
            Strawberries -> 8
            Tomatoes -> 16
            Chocolate -> 32
            Pollen -> 64
            Cats -> 128
    in
        (maskedScore .&. allergenValue) == allergenValue
