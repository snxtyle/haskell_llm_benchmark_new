module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, findIndex, splitAt)
import Control.Monad (guard)

-- | The five possible nationalities of the residents.
data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

-- | The solution to the puzzle, containing the resident who drinks water
--   and the resident who owns the zebra.
data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- | The five possible colors of the houses.
data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

-- | The five possible pets.
data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

-- | The five possible beverages.
data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

-- | The five possible brands of cigarettes.
data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

-- | A list of all possible colors.
colors :: [Color]
colors = [Red, Green, Ivory, Yellow, Blue]

-- | A list of all possible residents.
residents :: [Resident]
residents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]

-- | A list of all possible pets.
pets :: [Pet]
pets = [Dog, Snails, Fox, Horse, Zebra]

-- | A list of all possible drinks.
drinks :: [Drink]
drinks = [Coffee, Tea, Milk, OrangeJuice, Water]

-- | A list of all possible smoke brands.
smokes :: [Smoke]
smokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]

-- | Checks if two house indices (0-based) are next to each other.
nextTo :: Int -> Int -> Bool
nextTo a b = abs (a - b) == 1

-- | Generates all valid permutations of residents, where the Norwegian is in the first house.
validResidents :: [[Resident]]
validResidents = map (Norwegian :) (permutations [Englishman, Spaniard, Ukrainian, Japanese])

-- | Generates all valid permutations of colors, where the green house is immediately to the right of the ivory house.
validColors :: [[Color]]
validColors = do
  let otherColors = [Red, Yellow, Blue]
  pos <- [0..3] -- The (Ivory, Green) block can start at position 0, 1, 2, or 3.
  otherPerm <- permutations otherColors
  let (before, after) = splitAt pos otherPerm
  return $ before ++ [Ivory, Green] ++ after

-- | Generates all valid permutations of drinks, where milk is in the middle house.
validDrinks :: [[Drink]]
validDrinks = filter (\d -> d !! 2 == Milk) (permutations drinks)

-- | Solves the Zebra Puzzle.
--   It finds the unique arrangement of houses and their properties that
--   satisfies all 15 clues, then extracts the required information.
solve :: Solution
solve =
  let
    -- Find the unique solution tuple by filtering pre-filtered permutations.
    -- Constraints are applied incrementally to prune the search space.
    solutions = [ (colors', residents', pets', drinks', smokes') |
        -- 1. Generate residents with constraint 10 applied.
        residents' <- validResidents,

        -- 2. Generate colors with constraint 6 applied, and then filter with constraints 2 and 15.
        colors' <- validColors,
        let Just englishmanIdx = findIndex (== Englishman) residents'
        let Just redIdx = findIndex (== Red) colors'
        let Just blueIdx = findIndex (== Blue) colors'
        guard (englishmanIdx == redIdx),
        guard (nextTo 0 blueIdx), -- Norwegian is at index 0

        -- 3. Generate drinks with constraint 9 applied, and then filter with constraints 4 and 5.
        drinks' <- validDrinks,
        let Just ukrainianIdx = findIndex (== Ukrainian) residents'
        let Just teaIdx = findIndex (== Tea) drinks'
        let Just coffeeIdx = findIndex (== Coffee) drinks'
        let Just greenIdx = findIndex (== Green) colors'
        guard (ukrainianIdx == teaIdx),
        guard (coffeeIdx == greenIdx),

        -- 4. Generate pets and filter with constraint 3.
        pets' <- permutations pets,
        let Just spaniardIdx = findIndex (== Spaniard) residents'
        let Just dogIdx = findIndex (== Dog) pets'
        guard (spaniardIdx == dogIdx),

        -- 5. Generate smokes and filter with constraint 14.
        smokes' <- permutations smokes,
        let Just japaneseIdx = findIndex (== Japanese) residents'
        let Just parliamentsIdx = findIndex (== Parliaments) smokes'
        guard (japaneseIdx == parliamentsIdx),

        -- 6. Apply remaining constraints that link the remaining lists.
        let Just oldGoldIdx = findIndex (== OldGold) smokes'
        let Just snailsIdx = findIndex (== Snails) pets'
        let Just koolsIdx = findIndex (== Kools) smokes'
        let Just yellowIdx = findIndex (== Yellow) colors'
        let Just chesterfieldsIdx = findIndex (== Chesterfields) smokes'
        let Just foxIdx = findIndex (== Fox) pets'
        let Just horseIdx = findIndex (== Horse) pets'
        let Just luckyStrikeIdx = findIndex (== LuckyStrike) smokes'
        let Just orangeJuiceIdx = findIndex (== OrangeJuice) drinks'

        -- Constraint 7: The Old Gold smoker owns snails.
        guard (oldGoldIdx == snailsIdx),
        -- Constraint 8: Kools are smoked in the yellow house.
        guard (koolsIdx == yellowIdx),
        -- Constraint 11: The man who smokes Chesterfields lives in the house next to the man with the fox.
        guard (nextTo chesterfieldsIdx foxIdx),
        -- Constraint 12: Kools are smoked in the house next to the house where the horse is kept.
        guard (nextTo koolsIdx horseIdx),
        -- Constraint 13: The Lucky Strike smoker drinks orange juice.
        guard (luckyStrikeIdx == orangeJuiceIdx)
      ]

    -- Extract the single valid solution from the list.
    (colors_sol, residents_sol, pets_sol, drinks_sol, smokes_sol) = head solutions

    -- Find the index of the resident who drinks water.
    Just waterDrinkerIdx = findIndex (== Water) drinks_sol
    waterDrinkerResident = residents_sol !! waterDrinkerIdx

    -- Find the index of the resident who owns the zebra.
    Just zebraOwnerIdx = findIndex (== Zebra) pets_sol
    zebraOwnerResident = residents_sol !! zebraOwnerIdx

  in Solution { waterDrinker = waterDrinkerResident, zebraOwner = zebraOwnerResident }
