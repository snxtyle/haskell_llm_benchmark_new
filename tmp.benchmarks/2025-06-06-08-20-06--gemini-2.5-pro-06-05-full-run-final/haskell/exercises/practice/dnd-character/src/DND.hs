module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Data.List (sort)
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
modifier n = (n - 10) `div` 2

ability :: Gen Int
ability = fmap (sum . drop 1 . sort) (vectorOf 4 (choose (1, 6)))

character :: Gen Character
character = do
  s <- ability
  d <- ability
  c <- ability
  i <- ability
  w <- ability
  a <- ability
  return Character { strength = s
                   , dexterity = d
                   , constitution = c
                   , intelligence = i
                   , wisdom = w
                   , charisma = a
                   , hitpoints = 10 + modifier c
                   }
