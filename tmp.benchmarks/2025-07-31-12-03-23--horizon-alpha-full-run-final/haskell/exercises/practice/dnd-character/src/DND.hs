module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen)
import Test.QuickCheck (choose, vectorOf)
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
modifier n = (n - 10) `div` 2

ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1, 6))
  let topThree = drop 1 (sort rolls)
  pure (sum topThree)

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
        { strength = str
        , dexterity = dex
        , constitution = con
        , intelligence = intell
        , wisdom = wis
        , charisma = cha
        , hitpoints = hp
        }
