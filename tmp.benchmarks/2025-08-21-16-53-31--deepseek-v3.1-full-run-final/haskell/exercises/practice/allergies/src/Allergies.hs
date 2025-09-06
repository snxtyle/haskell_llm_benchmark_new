module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (Bits, (.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

allergyValues :: [(Allergen, Int)]
allergyValues = zip [minBound..maxBound] [1, 2, 4, 8, 16, 32, 64, 128]

allergies :: Int -> [Allergen]
allergies score = [allergen | (allergen, value) <- allergyValues, score `hasAllergen` value]
  where hasAllergen total value = total .&. value == value

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score `hasAllergen` value
  where Just value = lookup allergen allergyValues
        hasAllergen total val = total .&. val == val
