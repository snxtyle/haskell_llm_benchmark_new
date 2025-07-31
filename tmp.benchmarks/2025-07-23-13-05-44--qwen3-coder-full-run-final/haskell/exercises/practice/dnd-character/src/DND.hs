module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, listOf, suchThat)
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
modifier ability = floor (fromIntegral (ability - 10) / 2)

ability :: Gen Int
ability = do
  dice <- listOf (choose (1, 6)) `suchThat` (\xs -> length xs == 4)
  return $ sum dice - minimum dice

character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return $ Character str dex con int wis cha hp
