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

valueOf :: Allergen -> Int
valueOf Eggs         = 1
valueOf Peanuts      = 2
valueOf Shellfish    = 4
valueOf Strawberries = 8
valueOf Tomatoes     = 16
valueOf Chocolate    = 32
valueOf Pollen       = 64
valueOf Cats         = 128

allergies :: Int -> [Allergen]
allergies score = [ a | a <- allAllergens, isAllergicTo a score ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. valueOf allergen) /= 0
