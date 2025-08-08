module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, elemIndex)
import Control.Monad (guard)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Indices for each attribute category (fixed orderings)
-- Residents
englishmanI, spaniardI, ukrainianI, norwegianI, japaneseI :: Int
englishmanI = 0
spaniardI   = 1
ukrainianI  = 2
norwegianI  = 3
japaneseI   = 4

-- Colors
redI, greenI, ivoryI, yellowI, blueI :: Int
redI    = 0
greenI  = 1
ivoryI  = 2
yellowI = 3
blueI   = 4

-- Drinks
coffeeI, teaI, milkI, orangeJuiceI, waterI :: Int
coffeeI      = 0
teaI         = 1
milkI        = 2
orangeJuiceI = 3
waterI       = 4

-- Cigarettes
oldGoldI, koolsI, chesterfieldsI, luckyStrikeI, parliamentsI :: Int
oldGoldI      = 0
koolsI        = 1
chesterfieldsI = 2
luckyStrikeI  = 3
parliamentsI  = 4

-- Pets
dogI, snailsI, foxI, horseI, zebraI :: Int
dogI    = 0
snailsI = 1
foxI    = 2
horseI  = 3
zebraI  = 4

solve :: Solution
solve = head solutions

solutions :: [Solution]
solutions = do
  -- Each list is a permutation of house positions [1..5], where each index
  -- corresponds to a value in the fixed ordering defined above.
  -- For example, colors !! redI gives the house number that is red.
  colors <- permutations [1 .. 5]
  -- 6. Green is immediately to the right of ivory.
  guard (colors !! greenI == colors !! ivoryI + 1)

  nats <- permutations [1 .. 5]
  -- 10. Norwegian in the first house.
  guard (nats !! norwegianI == 1)
  -- 2. Englishman lives in the red house.
  guard (nats !! englishmanI == colors !! redI)
  -- 15. Norwegian lives next to the blue house.
  guard (adjacent (nats !! norwegianI) (colors !! blueI))

  drinks <- permutations [1 .. 5]
  -- 9. Milk in the middle house.
  guard (drinks !! milkI == 3)
  -- 4. Coffee in the green house.
  guard (drinks !! coffeeI == colors !! greenI)
  -- 5. Ukrainian drinks tea.
  guard (nats !! ukrainianI == drinks !! teaI)

  smokes <- permutations [1 .. 5]
  -- 8. Kools in the yellow house.
  guard (smokes !! koolsI == colors !! yellowI)
  -- 14. Japanese smokes Parliaments.
  guard (nats !! japaneseI == smokes !! parliamentsI)
  -- 13. Lucky Strike smoker drinks orange juice.
  guard (smokes !! luckyStrikeI == drinks !! orangeJuiceI)

  pets <- permutations [1 .. 5]
  -- 3. Spaniard owns the dog.
  guard (nats !! spaniardI == pets !! dogI)
  -- 7. Old Gold smoker owns snails.
  guard (smokes !! oldGoldI == pets !! snailsI)
  -- 11. Chesterfields next to the fox.
  guard (adjacent (smokes !! chesterfieldsI) (pets !! foxI))
  -- 12. Kools next to the horse.
  guard (adjacent (smokes !! koolsI) (pets !! horseI))

  let wHouse = drinks !! waterI
      zHouse = pets   !! zebraI
      wRes   = toResident (indexOf wHouse nats)
      zRes   = toResident (indexOf zHouse nats)
  pure (Solution wRes zRes)

adjacent :: Int -> Int -> Bool
adjacent a b = abs (a - b) == 1

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs =
  case elemIndex x xs of
    Just i  -> i
    Nothing -> error "Value not found (this should be impossible if constraints are correct)."

toResident :: Int -> Resident
toResident 0 = Englishman
toResident 1 = Spaniard
toResident 2 = Ukrainian
toResident 3 = Norwegian
toResident 4 = Japanese
toResident _ = error "Invalid resident index"
