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

allergies :: Int -> [Allergen]
allergies score = 
  let filteredScore = score .&. 255
      checkAllergen bit allergen = if filteredScore .&. bit /= 0 then [allergen] else []
  in concatMap (uncurry checkAllergen) allergensList
  where
    allergensList = [ (1, Eggs)
                    , (2, Peanuts)
                    , (4, Shellfish)
                    , (8, Strawberries)
                    , (16, Tomatoes)
                    , (32, Chocolate)
                    , (64, Pollen)
                    , (128, Cats)
                    ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = 
  case allergen of
    Eggs         -> score .&. 1   /= 0
    Peanuts      -> score .&. 2   /= 0
    Shellfish    -> score .&. 4   /= 0
    Strawberries -> score .&. 8   /= 0
    Tomatoes     -> score .&. 16  /= 0
    Chocolate    -> score .&. 32  /= 0
    Pollen       -> score .&. 64  /= 0
    Cats         -> score .&. 128 /= 0
