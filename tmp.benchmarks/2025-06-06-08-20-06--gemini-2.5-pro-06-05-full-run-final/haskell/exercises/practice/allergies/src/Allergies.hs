module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

-- A helper function to get the score value for a given allergen.
-- Since we derived Enum, we can use fromEnum to get an integer for each
-- allergen (0 for Eggs, 1 for Peanuts, etc.) and use it as an exponent.
allergenScore :: Allergen -> Int
allergenScore allergen = 2 ^ fromEnum allergen

-- A list of all possible allergens.
-- By deriving Enum and Bounded for Allergen, we can easily generate this list.
allAllergens :: [Allergen]
allAllergens = [minBound .. maxBound]

-- Determines the list of allergens for a given allergy score.
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) allAllergens

-- Determines if a person is allergic to a specific allergen given their score.
-- This works by using a bitwise AND with the allergen's score. If the result
-- is non-zero, the person is allergic.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score .&. allergenScore allergen /= 0
