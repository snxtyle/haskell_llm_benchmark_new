module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (Bits((.&.)))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

-- | A list of all possible allergens, in order of their value.
allAllergens :: [Allergen]
allAllergens = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

-- | Maps an Allergen to its corresponding integer value.
allergenValue :: Allergen -> Int
allergenValue Eggs        = 1
allergenValue Peanuts     = 2
allergenValue Shellfish   = 4
allergenValue Strawberries = 8
allergenValue Tomatoes    = 16
allergenValue Chocolate   = 32
allergenValue Pollen      = 64
allergenValue Cats        = 128

-- | Given an allergy score and an allergen, determine if the person is allergic to it.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenValue allergen) > 0

-- | Given an allergy score, return the list of all allergies.
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) allAllergens
