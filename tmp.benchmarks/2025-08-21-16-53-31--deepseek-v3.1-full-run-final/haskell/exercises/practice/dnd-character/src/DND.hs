module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)
import Test.QuickCheck (choose)
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

modifier :: Int -> Int
modifier score = (score - 10) `div` 2

ability :: Gen Int
ability = do
  diceRolls <- replicateM 4 (choose (1, 6))
  let sortedRolls = reverse (sort diceRolls)
  return $ sum (take 3 sortedRolls)

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hitpoints' = 10 + modifier con
  return $ Character str dex con int wis cha hitpoints'
