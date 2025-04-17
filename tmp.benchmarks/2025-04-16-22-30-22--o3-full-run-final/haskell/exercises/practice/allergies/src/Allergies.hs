module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

-- | Alérgenos posibles y su valor asociado:
--   Eggs        1
--   Peanuts     2
--   Shellfish   4
--   Strawberries 8
--   Tomatoes    16
--   Chocolate   32
--   Pollen      64
--   Cats        128
--
-- Derivamos Enum y Bounded para poder generar la lista completa
-- con [minBound .. maxBound].
data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

-- | Devuelve la lista de alérgenos presentes en el score.
allergies :: Int -> [Allergen]
allergies score = filter (`isAllergicTo` score) [minBound .. maxBound]

-- | Indica si el score contiene el alérgeno dado.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. scoreValue allergen) /= 0

-- | Valor numérico (potencia de dos) asociado a cada alérgeno.
scoreValue :: Allergen -> Int
scoreValue Eggs         = 1
scoreValue Peanuts      = 2
scoreValue Shellfish    = 4
scoreValue Strawberries = 8
scoreValue Tomatoes     = 16
scoreValue Chocolate    = 32
scoreValue Pollen       = 64
scoreValue Cats         = 128
