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

-- List of all allergens and their corresponding score values.
-- The order matches the Enum instance derived for Allergen.
allergenScores :: [(Allergen, Int)]
allergenScores =
  [ (Eggs, 1)
  , (Peanuts, 2)
  , (Shellfish, 4)
  , (Strawberries, 8)
  , (Tomatoes, 16)
  , (Chocolate, 32)
  , (Pollen, 64)
  , (Cats, 128)
  ]

-- Helper function to get the score value for a single allergen.
allergenScoreValue :: Allergen -> Int
allergenScoreValue allergen = case allergen of
    Eggs         -> 1
    Peanuts      -> 2
    Shellfish    -> 4
    Strawberries -> 8
    Tomatoes     -> 16
    Chocolate    -> 32
    Pollen       -> 64
    Cats         -> 128

-- Given a score, return the list of allergens for that score.
allergies :: Int -> [Allergen]
allergies score = [allergen | (allergen, value) <- allergenScores, score .&. value /= 0]

-- Given an allergen and a score, determine if the allergen is present in the score.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score .&. allergenScoreValue allergen /= 0
