module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List  (permutations, elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (guard)

-------------------------------------------------------------------------------
-- Public types ---------------------------------------------------------------
-------------------------------------------------------------------------------

data Resident
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Internal modelling of the houses ------------------------------------------
-------------------------------------------------------------------------------

data Color
  = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Drink
  = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Smoke
  = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data Pet
  = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

-------------------------------------------------------------------------------
-- Helpers --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Are the two (possibly absent) indices next to each other?
adjacent :: Maybe Int -> Maybe Int -> Bool
adjacent (Just a) (Just b) = abs (a - b) == 1
adjacent _ _               = False

idxOf :: Eq a => a -> [a] -> Maybe Int
idxOf = elemIndex

allResidents :: [Resident]
allResidents = [minBound .. maxBound]

allColors :: [Color]
allColors = [minBound .. maxBound]

allDrinks :: [Drink]
allDrinks = [minBound .. maxBound]

allSmokes :: [Smoke]
allSmokes = [minBound .. maxBound]

allPets :: [Pet]
allPets = [minBound .. maxBound]

-------------------------------------------------------------------------------
-- Search for all solutions (there is exactly one) ----------------------------
-------------------------------------------------------------------------------

solutions :: [(Resident, Resident)]
solutions = do
  colors <- permutations allColors
  -- 6. Green immediately to the right of ivory
  guard (idxOf Green colors == fmap (+1) (idxOf Ivory colors))

  residents <- permutations allResidents
  -- 10. Norwegian in the first house
  guard (idxOf Norwegian residents == Just 0)
  -- 2. Englishman in the red house
  guard (idxOf Englishman residents == idxOf Red colors)
  -- 15. Norwegian next to blue house
  guard (adjacent (idxOf Norwegian residents) (idxOf Blue colors))

  drinks <- permutations allDrinks
  -- 4. Coffee in green house
  guard (idxOf Coffee drinks == idxOf Green colors)
  -- 5. Ukrainian drinks tea
  guard (idxOf Ukrainian residents == idxOf Tea drinks)
  -- 9. Milk in the middle house
  guard (drinks !! 2 == Milk)

  smokes <- permutations allSmokes
  -- 8. Kools smoked in yellow house
  guard (idxOf Kools smokes == idxOf Yellow colors)
  -- 13. Lucky Strike smoker drinks orange juice
  guard (idxOf LuckyStrike smokes == idxOf OrangeJuice drinks)
  -- 14. Japanese smokes Parliaments
  guard (idxOf Japanese residents == idxOf Parliaments smokes)

  pets <- permutations allPets
  -- 3. Spaniard owns the dog
  guard (idxOf Spaniard residents == idxOf Dog pets)
  -- 7. Old Gold smoker owns snails
  guard (idxOf OldGold smokes == idxOf Snails pets)
  -- 11. Chesterfields smoker next to fox owner
  guard (adjacent (idxOf Chesterfields smokes) (idxOf Fox pets))
  -- 12. Kools smoker next to horse owner
  guard (adjacent (idxOf Kools smokes) (idxOf Horse pets))

  -- Derive final answers
  let waterIdx = fromJust (idxOf Water drinks)
      zebraIdx = fromJust (idxOf Zebra pets)
      waterGuy = residents !! waterIdx
      zebraGuy = residents !! zebraIdx
  return (waterGuy, zebraGuy)

-------------------------------------------------------------------------------
-- Public entry point ---------------------------------------------------------
-------------------------------------------------------------------------------

solve :: Solution
solve = case solutions of
          (w, z) : _ -> Solution w z
          []         -> error "No solution found (this should not happen)"
