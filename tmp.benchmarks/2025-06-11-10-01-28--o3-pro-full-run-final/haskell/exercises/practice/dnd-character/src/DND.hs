module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Control.Monad (replicateM)
import Test.QuickCheck (Gen, choose)

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
--   The modifier is (score - 10) divided by 2, rounded down.
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- | Generator for a single ability score.
--   Roll four 6-sided dice, drop the lowest roll, and sum the rest.
ability :: Gen Int
ability = do
  rolls <- replicateM 4 (choose (1, 6))
  pure (sum rolls - minimum rolls)

-- | Generator for a complete character with all six abilities
--   and derived hit points.
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  intell <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  pure Character
    { strength     = str
    , dexterity    = dex
    , constitution = con
    , intelligence = intell
    , wisdom       = wis
    , charisma     = cha
    , hitpoints    = hp
    }
