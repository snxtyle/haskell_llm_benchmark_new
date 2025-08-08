module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)

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
modifier s = (s - 10) `div` 2

ability :: Gen Int
ability = do
  dice <- vectorOf 4 (choose (1, 6))
  pure (sum dice - minimum dice)

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  pure Character
    { strength     = str
    , dexterity    = dex
    , constitution = con
    , intelligence = int
    , wisdom       = wis
    , charisma     = cha
    , hitpoints    = hp
    }
