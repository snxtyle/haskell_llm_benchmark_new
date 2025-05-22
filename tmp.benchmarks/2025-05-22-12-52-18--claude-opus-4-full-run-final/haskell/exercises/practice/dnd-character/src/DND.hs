module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose)
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
  -- Roll 4 six-sided dice
  rolls <- replicateM 4 (choose (1, 6))
  -- Sort and take the sum of the largest 3
  return $ sum $ drop 1 $ sort rolls

character :: Gen Character
character = do
  -- Generate all six abilities
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  -- Calculate hitpoints: 10 + constitution modifier
  let hp = 10 + modifier con
  return $ Character str dex con int wis cha hp
