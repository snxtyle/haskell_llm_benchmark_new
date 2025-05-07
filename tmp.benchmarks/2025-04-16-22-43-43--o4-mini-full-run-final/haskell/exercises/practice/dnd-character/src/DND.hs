module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)
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

-- | Given an ability score, compute its modifier:
-- subtract 10, divide by 2 and round down.
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- | Roll four six-sided dice, discard the lowest, and sum the remaining three.
ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6))
  let topThree = drop 1 (sort rolls)
  return (sum topThree)

-- | Generate a random character with six abilities and hitpoints.
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
