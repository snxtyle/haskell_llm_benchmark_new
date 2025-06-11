module ZebraPuzzle
  ( Resident (..)
  , Solution (..)
  , solve
  ) where

import Data.List  (permutations, elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (guard)

-- | Helper: safe index lookup (we assume presence due to well-formed permutations)
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = fromJust (elemIndex x xs)

-- | Two indices are neighbours if they differ by exactly one
adjacent :: Int -> Int -> Bool
adjacent a b = abs (a - b) == 1

----------------------------------------------------------------------
-- Domain definitions
----------------------------------------------------------------------

data Resident
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color
  = Red
  | Green
  | Ivory
  | Yellow
  | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet
  = Dog
  | Snails
  | Fox
  | Horse
  | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Drink
  = Coffee
  | Tea
  | Milk
  | OrangeJuice
  | Water
  deriving (Eq, Show, Enum, Bounded)

data Smoke
  = OldGold
  | Kools
  | Chesterfields
  | LuckyStrike
  | Parliaments
  deriving (Eq, Show, Enum, Bounded)

----------------------------------------------------------------------
-- Result type supplied by exercise
----------------------------------------------------------------------

data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

----------------------------------------------------------------------
-- Solver
----------------------------------------------------------------------

solve :: Solution
solve = head solutions
  where
    solutions :: [Solution]
    solutions = do
      -- Generate permutations for each attribute set, applying constraints
      colors <- permutations allColors
      let idxGreen = indexOf Green colors
          idxIvory = indexOf Ivory colors
      -- 6. The green house is immediately to the right of the ivory house.
      guard (idxGreen == idxIvory + 1)

      residents <- permutations allResidents
      let idxEnglishman = indexOf Englishman residents
          idxNorwegian  = indexOf Norwegian  residents
      -- 2. The Englishman lives in the red house.
      guard (idxEnglishman == indexOf Red colors)
      -- 10. The Norwegian lives in the first house.
      guard (idxNorwegian == 0)
      -- 15. The Norwegian lives next to the blue house.
      guard (adjacent idxNorwegian (indexOf Blue colors))

      drinks <- permutations allDrinks
      let idxCoffee = indexOf Coffee drinks
      -- 4. Coffee is drunk in the green house.
      guard (idxCoffee == idxGreen)
      -- 5. The Ukrainian drinks tea.
      guard (indexOf Ukrainian residents == indexOf Tea drinks)
      -- 9. Milk is drunk in the middle house. (house indices are 0-based here)
      guard (indexOf Milk drinks == 2)

      smokes <- permutations allSmokes
      -- 8. Kools are smoked in the yellow house.
      guard (indexOf Kools smokes == indexOf Yellow colors)
      -- 13. The Lucky Strike smoker drinks orange juice.
      guard (indexOf LuckyStrike smokes == indexOf OrangeJuice drinks)
      -- 14. The Japanese smokes Parliaments.
      guard (indexOf Japanese residents == indexOf Parliaments smokes)

      pets <- permutations allPets
      -- 3. The Spaniard owns the dog.
      guard (indexOf Spaniard residents == indexOf Dog pets)
      -- 7. The Old Gold smoker owns snails.
      guard (indexOf OldGold smokes == indexOf Snails pets)
      -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
      guard (adjacent (indexOf Chesterfields smokes) (indexOf Fox pets))
      -- 12. Kools are smoked in the house next to the house where the horse is kept.
      guard (adjacent (indexOf Kools smokes) (indexOf Horse pets))

      -- Passed all constraints; determine requested residents
      let waterIdx  = indexOf Water drinks
          zebraIdx  = indexOf Zebra pets
      pure Solution
            { waterDrinker = residents !! waterIdx
            , zebraOwner   = residents !! zebraIdx
            }

----------------------------------------------------------------------
-- Convenience lists
----------------------------------------------------------------------

allResidents :: [Resident]
allResidents = [minBound .. maxBound]

allColors :: [Color]
allColors = [minBound .. maxBound]

allPets :: [Pet]
allPets = [minBound .. maxBound]

allDrinks :: [Drink]
allDrinks = [minBound .. maxBound]

allSmokes :: [Smoke]
allSmokes = [minBound .. maxBound]
