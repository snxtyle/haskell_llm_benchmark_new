module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data House = House { color :: Color
                   , resident :: Resident
                   , pet :: Pet
                   , drink :: Drink
                   , smoke :: Smoke
                   } deriving (Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper function to zip 5 lists together
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a, b, c, d, e) : zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

-- Generate all possible solutions by creating permutations of each attribute
allSolutions :: [[House]]
allSolutions = 
  [ [ House c r p d s | (c, r, p, d, s) <- zip5 cs rs ps ds ss ]
  | cs <- permutations [minBound .. maxBound]
  , rs <- permutations [minBound .. maxBound]
  , ps <- permutations [minBound .. maxBound]
  , ds <- permutations [minBound .. maxBound]
  , ss <- permutations [minBound .. maxBound]
  ]

-- Check if a solution satisfies all constraints
isValidSolution :: [House] -> Bool
isValidSolution houses = and
  [ -- 2. The Englishman lives in the red house.
    any (\h -> resident h == Englishman && color h == Red) houses
    -- 3. The Spaniard owns the dog.
  , any (\h -> resident h == Spaniard && pet h == Dog) houses
    -- 4. Coffee is drunk in the green house.
  , any (\h -> drink h == Coffee && color h == Green) houses
    -- 5. The Ukrainian drinks tea.
  , any (\h -> resident h == Ukrainian && drink h == Tea) houses
    -- 6. The green house is immediately to the right of the ivory house.
  , any (\i -> i < 4 && color (houses !! i) == Ivory && color (houses !! (i+1)) == Green) [0..3]
    -- 7. The Old Gold smoker owns snails.
  , any (\h -> smoke h == OldGold && pet h == Snails) houses
    -- 8. Kools are smoked in the yellow house.
  , any (\h -> smoke h == Kools && color h == Yellow) houses
    -- 9. Milk is drunk in the middle house.
  , drink (houses !! 2) == Milk
    -- 10. The Norwegian lives in the first house.
  , resident (houses !! 0) == Norwegian
    -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
  , any (\i -> i < 4 && smoke (houses !! i) == Chesterfields && pet (houses !! (i+1)) == Fox) [0..3] ||
    any (\i -> i > 0 && smoke (houses !! i) == Chesterfields && pet (houses !! (i-1)) == Fox) [1..4]
    -- 12. Kools are smoked in the house next to the house where the horse is kept.
  , any (\i -> i < 4 && smoke (houses !! i) == Kools && pet (houses !! (i+1)) == Horse) [0..3] ||
    any (\i -> i > 0 && smoke (houses !! i) == Kools && pet (houses !! (i-1)) == Horse) [1..4]
    -- 13. The Lucky Strike smoker drinks orange juice.
  , any (\h -> smoke h == LuckyStrike && drink h == OrangeJuice) houses
    -- 14. The Japanese smokes Parliaments.
  , any (\h -> resident h == Japanese && smoke h == Parliaments) houses
    -- 15. The Norwegian lives next to the blue house.
  , any (\i -> i < 4 && resident (houses !! i) == Norwegian && color (houses !! (i+1)) == Blue) [0..3] ||
    any (\i -> i > 0 && resident (houses !! i) == Norwegian && color (houses !! (i-1)) == Blue) [1..4]
  ]

-- Find the resident who drinks water
findWaterDrinker :: [House] -> Resident
findWaterDrinker houses = resident $ head $ filter (\h -> drink h == Water) houses

-- Find the resident who owns the zebra
findZebraOwner :: [House] -> Resident
findZebraOwner houses = resident $ head $ filter (\h -> pet h == Zebra) houses

-- Solve the puzzle
solve :: Solution
solve = 
  let validSolution = head $ filter isValidSolution allSolutions
      waterDrinker' = findWaterDrinker validSolution
      zebraOwner' = findZebraOwner validSolution
  in Solution waterDrinker' zebraOwner'
