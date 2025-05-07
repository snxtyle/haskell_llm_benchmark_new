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

-- Helper function to get the integer score for a given Allergen.
allergenScore :: Allergen -> Int
allergenScore Eggs         = 1
allergenScore Peanuts      = 2
allergenScore Shellfish    = 4
allergenScore Strawberries = 8
allergenScore Tomatoes     = 16
allergenScore Chocolate    = 32
allergenScore Pollen       = 64
allergenScore Cats         = 128

-- A list of all known allergens and their corresponding scores.
-- This list defines the allergens we care about and their checking order.
allergenItems :: [(Allergen, Int)]
allergenItems =
  [ (Eggs, 1)
  , (Peanuts, 2)
  , (Shellfish, 4)
  , (Strawberries, 8)
  , (Tomatoes, 16)
  , (Chocolate, 32)
  , (Pollen, 64)
  , (Cats, 128)
  ]

-- Given a score, return a list of all allergens the person is allergic to.
-- Allergens not in the known list (e.g., with scores 256, 512) are ignored.
allergies :: Int -> [Allergen]
allergies score =
  [allergen | (allergen, val) <- allergenItems, (score .&. val) /= 0]

-- Given an allergen and a score, determine if the person is allergic to that specific allergen.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
  let
    targetScore = allergenScore allergen
  in
    (score .&. targetScore) /= 0
