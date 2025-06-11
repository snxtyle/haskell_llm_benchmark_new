module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

-- | All allergens that the test cares about.
--   The order corresponds to their bit significance
--   (Eggs = 1, Peanuts = 2, …, Cats = 128).
data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

-- | Numeric value associated with an allergen.
allergenValue :: Allergen -> Int
allergenValue = (1 `shiftL`) . fromEnum
  where
    -- Local re-implementation of shiftL to avoid importing Data.Bits qualified
    shiftL x n = x * (2 ^ n)

-- | Return the list of allergens a person is allergic to, in ascending
--   order of their bit value.
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) [minBound .. maxBound]

-- | Determine if a person with the given score is allergic to the supplied
--   allergen.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score .&. allergenValue allergen /= 0
