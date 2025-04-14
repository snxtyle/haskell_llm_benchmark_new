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

allAllergens :: [(Allergen, Int)]
allAllergens =
  [ (Eggs, 1)
  , (Peanuts, 2)
  , (Shellfish, 4)
  , (Strawberries, 8)
  , (Tomatoes, 16)
  , (Chocolate, 32)
  , (Pollen, 64)
  , (Cats, 128)
  ]

allergies :: Int -> [Allergen]
allergies score = [ allergen | (allergen, value) <- allAllergens, score .&. value /= 0 ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
  case lookup allergen allAllergens of
    Just value -> (score .&. value) /= 0
    Nothing -> False
