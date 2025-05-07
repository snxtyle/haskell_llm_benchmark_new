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

-- List of all allergens in order, with their corresponding bit value
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

-- Use standard bitwise AND
allergies :: Int -> [Allergen]
allergies score = [ allergen | (allergen, val) <- allAllergens, score .&. val /= 0 ]
  where (.&.) = (.&)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
    case lookup allergen allAllergens of
        Just val -> score .&. val /= 0
        Nothing  -> False
  where (.&.) = (.&)

-- Use the standard bitwise AND from Prelude (for GHC >= 7.10, Data.Bits is in Prelude)
import Data.Bits ((.&.))
