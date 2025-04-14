module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, vectorOf, choose)
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

-- Calculate the modifier for an ability score
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- Roll four 6-sided dice, discard the lowest, sum the rest
ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1,6))
  let topThree = drop 1 (sort rolls)  -- sort ascending, drop lowest
  return (sum topThree)

-- Generate a character with six abilities and hitpoints
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
    { strength = str
    , dexterity = dex
    , constitution = con
    , intelligence = int
    , wisdom = wis
    , charisma = cha
    , hitpoints = hp
    }
