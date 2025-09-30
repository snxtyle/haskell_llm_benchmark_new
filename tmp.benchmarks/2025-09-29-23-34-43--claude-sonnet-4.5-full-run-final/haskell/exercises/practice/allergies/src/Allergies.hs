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
              deriving (Eq, Show)

-- Get the numeric value for each allergen
allergenValue :: Allergen -> Int
allergenValue Eggs         = 1
allergenValue Peanuts      = 2
allergenValue Shellfish    = 4
allergenValue Strawberries = 8
allergenValue Tomatoes     = 16
allergenValue Chocolate    = 32
allergenValue Pollen       = 64
allergenValue Cats         = 128

-- List of all allergens in order
allAllergens :: [Allergen]
allAllergens = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

-- Get all allergies from a score
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) allAllergens

-- Check if allergic to a specific allergen
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenValue allergen) /= 0
