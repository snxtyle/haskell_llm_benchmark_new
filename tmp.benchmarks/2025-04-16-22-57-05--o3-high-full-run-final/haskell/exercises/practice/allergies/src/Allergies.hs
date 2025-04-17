module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

-- | An allergen together with the numeric score used in the allergy test.
data Allergen
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats
    deriving (Eq, Show, Enum, Bounded)

-- | Numeric value associated with each allergen.
scoreOf :: Allergen -> Int
scoreOf Eggs         = 1
scoreOf Peanuts      = 2
scoreOf Shellfish    = 4
scoreOf Strawberries = 8
scoreOf Tomatoes     = 16
scoreOf Chocolate    = 32
scoreOf Pollen       = 64
scoreOf Cats         = 128

-- | List all allergens for which the given score has the corresponding bit set.
--
-- Allergens not present in 'Allergen' are ignored automatically, because we
-- only look at the eight lowest‑order bits that belong to our defined
-- allergens.
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) [minBound .. maxBound]

-- | Determine whether the given score indicates an allergy to the specified
-- allergen.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = score .&. scoreOf allergen /= 0
