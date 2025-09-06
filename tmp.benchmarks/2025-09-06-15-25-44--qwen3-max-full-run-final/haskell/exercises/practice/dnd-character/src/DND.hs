module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, elements, choose)
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

-- Calculate the ability modifier: subtract 10, divide by 2, round down
modifier :: Int -> Int
modifier score = floor (fromIntegral (score - 10) / 2)

-- Generate a single ability score by rolling 4d6 and summing the highest 3
ability :: Gen Int
ability = do
  rolls <- replicateM 4 (choose (1, 6))
  let sortedRolls = reverse $ sort rolls  -- Sort in descending order
  return $ sum $ take 3 sortedRolls       -- Take the highest 3 and sum them

-- Generate a complete character
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return $ Character
    { strength = str
    , dexterity = dex
    , constitution = con
    , intelligence = int
    , wisdom = wis
    , charisma = cha
    , hitpoints = hp
    }
