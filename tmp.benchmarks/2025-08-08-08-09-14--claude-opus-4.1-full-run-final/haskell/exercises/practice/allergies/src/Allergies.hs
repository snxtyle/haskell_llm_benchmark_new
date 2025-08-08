module Allergies (Allergen(..), allergies, isAllergicTo) where

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
allergies score = filter (`isAllergicTo` score) allAllergens
  where
    allAllergens = [Eggs, Peanuts, Shellfish, Strawberries, 
                    Tomatoes, Chocolate, Pollen, Cats]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score `mod` 256) .&. allergenValue allergen /= 0
  where
    allergenValue :: Allergen -> Int
    allergenValue Eggs         = 1
    allergenValue Peanuts      = 2
    allergenValue Shellfish    = 4
    allergenValue Strawberries = 8
    allergenValue Tomatoes     = 16
    allergenValue Chocolate    = 32
    allergenValue Pollen       = 64
    allergenValue Cats         = 128
    
    (.&.) :: Int -> Int -> Int
    (.&.) = bitAnd
    
    bitAnd :: Int -> Int -> Int
    bitAnd 0 _ = 0
    bitAnd _ 0 = 0
    bitAnd x y = if odd x && odd y
                 then 1 + 2 * bitAnd (x `div` 2) (y `div` 2)
                 else 2 * bitAnd (x `div` 2) (y `div` 2)
