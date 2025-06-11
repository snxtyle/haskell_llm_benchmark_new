module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose)
import Data.List (sort)
import Control.Monad (replicateM)

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
  dice <- replicateM 4 (choose (1, 6))
  return $ sum (tail (sort dice))

character :: Gen Character
character = do
  abilities <- replicateM 6 ability
  let [s, d, c, i, w, ch] = abilities
  let hp = 10 + modifier c
  return $ Character s d c i w ch hp
