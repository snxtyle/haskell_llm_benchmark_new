module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, elemIndex)
import Data.Maybe (fromJust)

-- Given resident types
data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

-- Internal attribute types
data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)
data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)
data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

-- The solution we expose
data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

solve :: Solution
solve = head
  [ Solution (residents !! waterIdx) (residents !! zebraIdx)
  | residents <- permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
  , let idxR r   = fromJust (elemIndex r residents)
  -- 10. The Norwegian lives in the first house.
  , idxR Norwegian == 0

  , colors <- permutations [Red, Green, Ivory, Yellow, Blue]
  , let idxC c   = fromJust (elemIndex c colors)
  -- 2. The Englishman lives in the red house.
  , colors !! idxR Englishman == Red
  -- 6. The green house is immediately to the right of the ivory house.
  , idxC Green == idxC Ivory + 1
  -- 15. The Norwegian lives next to the blue house.
  , abs (idxR Norwegian - idxC Blue) == 1

  , drinks <- permutations [Coffee, Tea, Milk, OrangeJuice, Water]
  , let idxD d   = fromJust (elemIndex d drinks)
  -- 5. The Ukrainian drinks tea.
  , drinks !! idxR Ukrainian == Tea
  -- 4. Coffee is drunk in the green house.
  , drinks !! idxC Green   == Coffee
  -- 9. Milk is drunk in the middle house.
  , drinks !! 2             == Milk

  , pets <- permutations [Dog, Snails, Fox, Horse, Zebra]
  , let idxP p   = fromJust (elemIndex p pets)
  -- 3. The Spaniard owns the dog.
  , pets !! idxR Spaniard   == Dog

  , smoke <- permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  , let idxS s   = fromJust (elemIndex s smoke)
  -- 7. The Old Gold smoker owns snails.
  , pets  !! idxS OldGold    == Snails
  -- 8. Kools are smoked in the yellow house.
  , colors!! idxS Kools      == Yellow
  -- 11. The man who smokes Chesterfields lives next to the man with the fox.
  , abs (idxS Chesterfields - idxP Fox)    == 1
  -- 12. Kools are smoked in the house next to the house where the horse is kept.
  , abs (idxS Kools         - idxP Horse)  == 1
  -- 13. The Lucky Strike smoker drinks orange juice.
  , drinks!! idxS LuckyStrike == OrangeJuice
  -- 14. The Japanese smokes Parliaments.
  , residents !! idxS Parliaments == Japanese

  -- Finally extract answers
  , let waterIdx = idxD Water
        zebraIdx = idxP Zebra
  ]
