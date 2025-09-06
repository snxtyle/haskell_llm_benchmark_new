module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

-- Convert an allergen to its corresponding score value
allergenValue :: Allergen -> Int
allergenValue Eggs = 1
allergenValue Peanuts = 2
allergenValue Shellfish = 4
allergenValue Strawberries = 8
allergenValue Tomatoes = 16
allergenValue Chocolate = 32
allergenValue Pollen = 64
allergenValue Cats = 128

-- Check if a person is allergic to a specific allergen
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score `mod` (2 * val) >= val
  where val = allergenValue allergen

-- Get the list of all allergens a person is allergic to
allergies :: Int -> [Allergen]
allergies score = filter (flip isAllergicTo score) [minBound .. maxBound]
