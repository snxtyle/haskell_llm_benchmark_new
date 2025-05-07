module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose) -- For Gen type and choose function
import Control.Monad (replicateM)   -- For replicateM to generate multiple dice rolls
import Data.List (sort)             -- For sorting dice rolls

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

-- Calculates the ability modifier.
-- For constitution: (constitution - 10) / 2, rounded down.
modifier :: Int -> Int
modifier score = floor (fromIntegral (score - 10) / 2.0 :: Double)

-- Generates a single ability score.
-- This involves rolling four 6-sided dice and summing the largest three.
ability :: Gen Int
ability = do
  -- Roll four 6-sided dice
  rolls <- replicateM 4 (choose (1, 6) :: Gen Int)
  -- Sort the rolls in ascending order
  let sortedRolls = sort rolls
  -- Sum the three largest dice (by dropping the smallest one)
  let sumOfLargestThree = sum (drop 1 sortedRolls)
  return sumOfLargestThree

-- Generates a character with random ability scores and calculated hitpoints.
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  
  let conModifier = modifier con
  let hp = 10 + conModifier
  
  return Character
    { strength     = str
    , dexterity    = dex
    , constitution = con
    , intelligence = int
    , wisdom       = wis
    , charisma     = cha
    , hitpoints    = hp
    }
