module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.), shiftL)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

allergenValue :: Allergen -> Int
allergenValue a = 1 `shiftL` fromEnum a

scoreMask :: Int
scoreMask = 0xFF

allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) [minBound .. maxBound]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
  ((score .&. scoreMask) .&. allergenValue allergen) /= 0
