module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (choose)
import Control.Monad (replicateM)
import Data.List (sort)

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

-- | Calculate the ability modifier for a given score.
-- The modifier is (score - 10) divided by 2, rounded down.
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- | Generate a random ability score.
-- An ability score is the sum of the highest three of four six-sided dice rolls.
ability :: Gen Int
ability = do
  -- Roll four 6-sided dice
  rolls <- replicateM 4 (choose (1, 6))
  -- Sum the three highest rolls
  return $ sum (tail (sort rolls))

-- | Generate a random D&D character.
-- A character has six ability scores and hitpoints calculated from their constitution.
character :: Gen Character
character = do
  -- Generate the six ability scores
  scores <- replicateM 6 ability
  let [s, d, c, i, w, ch] = scores
  -- Calculate hitpoints based on the constitution modifier
  let hp = 10 + modifier c
  -- Construct the character
  return $ Character
    { strength = s
    , dexterity = d
    , constitution = c
    , intelligence = i
    , wisdom = w
    , charisma = ch
    , hitpoints = hp
    }
