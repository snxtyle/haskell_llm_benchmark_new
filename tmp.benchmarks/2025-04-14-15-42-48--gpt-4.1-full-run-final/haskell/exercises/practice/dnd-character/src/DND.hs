module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen (choose, vectorOf)
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

-- Calculate the ability modifier
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- Generate a single ability score by rolling 4d6 and summing the top 3
ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1,6))
  let topThree = drop 1 (sort rolls) -- drop the lowest
  return (sum topThree)

-- Generate a full character with all abilities and hitpoints
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
