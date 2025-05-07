module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List  (permutations, elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (guard)

-- Public types ---------------------------------------------------------------

data Resident
  = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

-- Internal helpers -----------------------------------------------------------

data Color   = Red | Green | Ivory | Yellow | Blue      deriving (Eq, Show)
data Pet     = Dog | Snails | Fox | Horse | Zebra       deriving (Eq, Show)
data Drink   = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show)
data Smoke   = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

allResidents :: [Resident]
allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]

allColors :: [Color]
allColors = [Red, Green, Ivory, Yellow, Blue]

allPets :: [Pet]
allPets = [Dog, Snails, Fox, Horse, Zebra]

allDrinks :: [Drink]
allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]

allSmokes :: [Smoke]
allSmokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = fromJust (elemIndex x xs)

-- The actual solver ----------------------------------------------------------

solve :: Solution
solve = head solutions
  where
    solutions = do
      -- 10. The Norwegian lives in the first house.
      residents <- filter ((== Norwegian) . head) $ permutations allResidents
      let pos r = indexOf r residents
          norwegianPos = pos Norwegian
          englishPos   = pos Englishman
          ukrainianPos = pos Ukrainian
          japanesePos  = pos Japanese

      colors <- permutations allColors
      let posColor c = indexOf c colors
      -- 2. The Englishman lives in the red house.
      guard (posColor Red == englishPos)
      -- 6. The green house is immediately to the right of the ivory house.
      let ivoryPos = posColor Ivory
      guard (posColor Green == ivoryPos + 1)
      -- 15. The Norwegian lives next to the blue house.
      guard (abs (norwegianPos - posColor Blue) == 1)

      drinks <- permutations allDrinks
      let posDrink d = indexOf d drinks
      -- 9. Milk is drunk in the middle house.
      guard (drinks !! 2 == Milk)          -- index 2 == middle (third) house
      -- 4. Coffee is drunk in the green house.
      guard (posDrink Coffee == posColor Green)
      -- 5. The Ukrainian drinks tea.
      guard (posDrink Tea == ukrainianPos)

      smokes <- permutations allSmokes
      let posSmoke s = indexOf s smokes
      -- 14. The Japanese smokes Parliaments.
      guard (posSmoke Parliaments == japanesePos)
      -- 8. Kools are smoked in the yellow house.
      guard (posSmoke Kools == posColor Yellow)
      -- 13. The Lucky Strike smoker drinks orange juice.
      guard (posDrink OrangeJuice == posSmoke LuckyStrike)

      pets <- permutations allPets
      let posPet p = indexOf p pets
      -- 3. The Spaniard owns the dog.
      guard (posPet Dog == pos Spaniard)
      -- 7. The Old Gold smoker owns snails.
      guard (posPet Snails == posSmoke OldGold)
      -- 11. The Chesterfields smoker lives next to the man with the fox.
      guard (abs (posSmoke Chesterfields - posPet Fox) == 1)
      -- 12. Kools are smoked in the house next to the house where the horse is kept.
      guard (abs (posSmoke Kools - posPet Horse) == 1)

      pure Solution
        { waterDrinker = residents !! posDrink Water
        , zebraOwner   = residents !! posPet  Zebra
        }
