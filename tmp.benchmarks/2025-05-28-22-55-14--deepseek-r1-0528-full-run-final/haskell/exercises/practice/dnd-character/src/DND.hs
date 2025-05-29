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
modifier x = (x - 10) `div` 2

ability :: Gen Int
ability = do
  rolls <- vectorOf 4 (choose (1,6))
  return $ sum rolls - minimum rolls

character :: Gen Character
character = do
  s <- ability
  d <- ability
  c <- ability
  i <- ability
  w <- ability
  ch <- ability
  let hp = 10 + modifier c
  return $ Character s d c i w ch hp
