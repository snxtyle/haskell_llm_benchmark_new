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

-- List of all allergens with their corresponding bit values
allergenValues :: [(Allergen, Int)]
allergenValues = [ (Eggs, 1)
                 , (Peanuts, 2)
                 , (Shellfish, 4)
                 , (Strawberries, 8)
                 , (Tomatoes, 16)
                 , (Chocolate, 32)
                 , (Pollen, 64)
                 , (Cats, 128)
                 ]

-- Get the bit value for a specific allergen
allergenValue :: Allergen -> Int
allergenValue allergen = case lookup allergen allergenValues of
  Just value -> value
  Nothing -> 0

allergies :: Int -> [Allergen]
allergies score = [allergen | (allergen, value) <- allergenValues, score .&. value /= 0]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score .&. allergenValue allergen /= 0
