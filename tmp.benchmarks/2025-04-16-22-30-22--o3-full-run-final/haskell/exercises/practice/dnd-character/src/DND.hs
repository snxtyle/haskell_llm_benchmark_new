module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Control.Monad      (replicateM)
import Test.QuickCheck    (Gen, choose, vectorOf)

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

-- | Calculate the ability modifier for a given ability score.
--   The modifier is the floor of (score - 10) / 2.
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- | Generate a single ability score by rolling four six‑sided dice
--   and summing the highest three results.
ability :: Gen Int
ability = do
  dice <- vectorOf 4 (choose (1, 6))      -- roll four d6
  let total = sum dice - minimum dice     -- drop the lowest roll
  return total

-- | Generate a complete character with all six abilities and
--   hit points calculated from the constitution modifier.
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return Character
    { strength     = str
    , dexterity    = dex
    , constitution = con
    , intelligence = int
    , wisdom       = wis
    , charisma     = cha
    , hitpoints    = hp
    }
