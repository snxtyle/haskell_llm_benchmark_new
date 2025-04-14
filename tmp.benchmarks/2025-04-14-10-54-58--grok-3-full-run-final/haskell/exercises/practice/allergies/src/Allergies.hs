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

-- Helper function to map allergens to their scores
allergenScores :: [(Allergen, Int)]
allergenScores = [
    (Eggs, 1),
    (Peanuts, 2),
    (Shellfish, 4),
    (Strawberries, 8),
    (Tomatoes, 16),
    (Chocolate, 32),
    (Pollen, 64),
    (Cats, 128)
  ]

allergies :: Int -> [Allergen]
allergies score = filter (\allergen -> isAllergicTo allergen score) [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = 
  let allergenScore = lookup allergen allergenScores
  in case allergenScore of
       Just value -> (score .&. value) == value
       Nothing -> False
