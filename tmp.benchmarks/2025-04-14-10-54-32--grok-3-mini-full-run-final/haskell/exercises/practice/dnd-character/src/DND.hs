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

modifier :: Int -> Int
modifier score =
  (score - 10) `div` 2

ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6))  -- Generate four rolls of a 6-sided die
  let sortedRolls = sort rolls          -- Sort the rolls in ascending order
  let toSum = drop 1 sortedRolls       -- Drop the smallest roll
  return (sum toSum)                   -- Sum the remaining three rolls

character :: Gen Character
character = do
  strength     <- ability
  dexterity    <- ability
  constitution <- ability
  intelligence <- ability
  wisdom       <- ability
  charisma     <- ability
  let hp = 10 + modifier constitution  -- Calculate hitpoints using constitution modifier
  return (Character strength dexterity constitution intelligence wisdom charisma hp)
