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

-- Internal mapping from Allergen to its score value
allergenValue :: Allergen -> Int
allergenValue Eggs         = 1
allergenValue Peanuts      = 2
allergenValue Shellfish    = 4
allergenValue Strawberries = 8
allergenValue Tomatoes     = 16
allergenValue Chocolate    = 32
allergenValue Pollen       = 64
allergenValue Cats         = 128

-- List of all known allergens in defined order
allAllergens :: [Allergen]
allAllergens =
  [ Eggs
  , Peanuts
  , Shellfish
  , Strawberries
  , Tomatoes
  , Chocolate
  , Pollen
  , Cats
  ]

-- Returns True if the given score contains the bit for that allergen
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
  score .&. allergenValue allergen /= 0

-- Returns the list of all allergens whose bits are set in the score
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) allAllergens
