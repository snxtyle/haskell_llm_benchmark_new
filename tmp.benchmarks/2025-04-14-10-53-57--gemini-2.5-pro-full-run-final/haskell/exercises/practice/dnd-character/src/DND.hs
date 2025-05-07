module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)
import Control.Monad (replicateM)
import Data.List (sort)
import Data.Fixed (div') -- For floor division

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

-- Calculates the modifier for a given ability score.
-- Modifier = floor((score - 10) / 2)
modifier :: Int -> Int
modifier score = floor $ (fromIntegral (score - 10)) / (2 :: Double)
-- Alternative using integer division: (score - 10) `div` 2, but need to handle negative results correctly for floor division.
-- Haskell's `div` truncates towards zero, `quot` truncates towards negative infinity (floor division for positive divisors).
-- Let's stick to the floating point conversion and floor for clarity, or use `div'` from Data.Fixed if preferred.
-- Using `div` is actually correct here as per standard D&D rules interpretation. Let's switch to that for simplicity.
-- modifier score = (score - 10) `div` 2

-- Generates a single ability score.
-- Rolls 4d6, drops the lowest, sums the rest.
ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6) :: Gen Int)
  return $ sum . drop 1 . sort $ rolls

-- Generates a complete character.
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return Character { strength     = str
                   , dexterity    = dex
                   , constitution = con
                   , intelligence = int
                   , wisdom       = wis
                   , charisma     = cha
                   , hitpoints    = hp
                   }
