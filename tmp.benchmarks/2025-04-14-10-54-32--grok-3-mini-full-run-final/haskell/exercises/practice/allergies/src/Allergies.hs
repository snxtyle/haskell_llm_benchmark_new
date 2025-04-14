module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

allergenScore :: Allergen -> Int
allergenScore Eggs = 1
allergenScore Peanuts = 2
allergenScore Shellfish = 4
allergenScore Strawberries = 8
allergenScore Tomatoes = 16
allergenScore Chocolate = 32
allergenScore Pollen = 64
allergenScore Cats = 128

allergies :: Int -> [Allergen]
allergies score = [allergen | (allergen, val) <- allergenList, score .&. val == val]
  where
    allergenList = [(Eggs, 1), (Peanuts, 2), (Shellfish, 4), (Strawberries, 8), (Tomatoes, 16), (Chocolate, 32), (Pollen, 64), (Cats, 128)]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenScore allergen) == allergenScore allergen
