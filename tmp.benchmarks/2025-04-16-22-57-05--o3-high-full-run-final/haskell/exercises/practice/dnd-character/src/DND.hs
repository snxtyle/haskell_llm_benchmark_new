module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Control.Monad      (replicateM)
import Data.List          (minimum)
import Test.QuickCheck    (Gen, choose)

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

-- | Calcula el modificador de una puntuación de característica
--   (score - 10) / 2 redondeado hacia abajo.
modifier :: Int -> Int
modifier score = (score - 10) `div` 2

-- | Generador de una puntuación de característica según las reglas:
--   lanzar 4 dados de 6 caras y sumar los 3 mayores.
ability :: Gen Int
ability = do
  rolls <- replicateM 4 (choose (1, 6))  -- cuatro dados de 6 caras
  return $ sum rolls - minimum rolls      -- descartar el menor resultado

-- | Generador completo de un personaje.
character :: Gen Character
character = do
  str <- ability
  dex <- ability
  con <- ability
  int_ <- ability
  wis <- ability
  cha <- ability
  let hp = 10 + modifier con
  return Character
    { strength     = str
    , dexterity    = dex
    , constitution = con
    , intelligence = int_
    , wisdom       = wis
    , charisma     = cha
    , hitpoints    = hp
    }
