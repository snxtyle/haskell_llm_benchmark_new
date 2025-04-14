module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, find)
import Data.Maybe (listToMaybe) -- Used for safer list head access

-- Enumerations for house properties and positions
data HousePos = First | Second | Third | Fourth | Fifth deriving (Eq, Show, Enum, Bounded, Ord)
data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show, Enum, Bounded)
data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese deriving (Eq, Show, Enum, Bounded)
data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Eq, Show, Enum, Bounded)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show, Enum, Bounded)
data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Eq, Show, Enum, Bounded)

-- Structure representing a single house with all its properties
data House = House { pos :: HousePos
                   , color :: Color
                   , resident :: Resident
                   , pet :: Pet
                   , drink :: Drink
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

-- The solution grid is represented as a list of 5 houses
type SolutionGrid = [House]

-- The final answer format required by the problem
data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper function to find a house in the grid based on a specific property value.
-- Example: findHouse resident Englishman grid
findHouse :: (Eq a) => (House -> a) -> a -> SolutionGrid -> Maybe House
findHouse prop val = find (\h -> prop h == val)

-- Helper function to check if two houses are adjacent in the grid.
areNeighbors :: House -> House -> Bool
areNeighbors h1 h2 = abs (fromEnum (pos h1) - fromEnum (pos h2)) == 1

-- Helper function to check if house h1 is immediately to the right of house h2.
isRightOf :: House -> House -> Bool
isRightOf h1 h2 = fromEnum (pos h1) == fromEnum (pos h2) + 1

-- Constraint Implementations (Rules 2-15)
-- Rule 1 (5 houses) is implicit in the SolutionGrid structure and generation.

-- Rule 2: The Englishman lives in the red house.
rule2 :: SolutionGrid -> Bool
rule2 grid = maybe False (\h -> color h == Red) $ findHouse resident Englishman grid

-- Rule 3: The Spaniard owns the dog.
rule3 :: SolutionGrid -> Bool
rule3 grid = maybe False (\h -> pet h == Dog) $ findHouse resident Spaniard grid

-- Rule 4: Coffee is drunk in the green house.
rule4 :: SolutionGrid -> Bool
rule4 grid = maybe False (\h -> drink h == Coffee) $ findHouse color Green grid

-- Rule 5: The Ukrainian drinks tea.
rule5 :: SolutionGrid -> Bool
rule5 grid = maybe False (\h -> drink h == Tea) $ findHouse resident Ukrainian grid

-- Rule 6: The green house is immediately to the right of the ivory house.
rule6 :: SolutionGrid -> Bool
rule6 grid = case (findHouse color Green grid, findHouse color Ivory grid) of
               (Just greenHouse, Just ivoryHouse) -> isRightOf greenHouse ivoryHouse
               _ -> False -- One or both houses not found

-- Rule 7: The Old Gold smoker owns snails.
rule7 :: SolutionGrid -> Bool
rule7 grid = maybe False (\h -> pet h == Snails) $ findHouse cigarette OldGold grid

-- Rule 8: Kools are smoked in the yellow house.
rule8 :: SolutionGrid -> Bool
rule8 grid = maybe False (\h -> cigarette h == Kools) $ findHouse color Yellow grid

-- Rule 9: Milk is drunk in the middle house (Third position).
rule9 :: SolutionGrid -> Bool
rule9 grid = maybe False (\h -> drink h == Milk) $ findHouse pos Third grid

-- Rule 10: The Norwegian lives in the first house.
rule10 :: SolutionGrid -> Bool
rule10 grid = maybe False (\h -> resident h == Norwegian) $ findHouse pos First grid

-- Rule 11: The man who smokes Chesterfields lives in the house next to the man with the fox.
rule11 :: SolutionGrid -> Bool
rule11 grid = case (findHouse cigarette Chesterfields grid, findHouse pet Fox grid) of
                (Just chesterfieldsSmoker, Just foxOwner) -> areNeighbors chesterfieldsSmoker foxOwner
                _ -> False -- One or both residents not found

-- Rule 12: Kools are smoked in the house next to the house where the horse is kept.
rule12 :: SolutionGrid -> Bool
rule12 grid = case (findHouse cigarette Kools grid, findHouse pet Horse grid) of
                 (Just koolsSmoker, Just horseOwner) -> areNeighbors koolsSmoker horseOwner
                 _ -> False -- One or both houses not found

-- Rule 13: The Lucky Strike smoker drinks orange juice.
rule13 :: SolutionGrid -> Bool
rule13 grid = maybe False (\h -> drink h == OrangeJuice) $ findHouse cigarette LuckyStrike grid

-- Rule 14: The Japanese smokes Parliaments.
rule14 :: SolutionGrid -> Bool
rule14 grid = maybe False (\h -> cigarette h == Parliaments) $ findHouse resident Japanese grid

-- Rule 15: The Norwegian lives next to the blue house.
rule15 :: SolutionGrid -> Bool
rule15 grid = case (findHouse resident Norwegian grid, findHouse color Blue grid) of
                (Just norwegian, Just blueHouse) -> areNeighbors norwegian blueHouse
                _ -> False -- One or both houses not found

-- List containing all the rule functions for easy application.
allRules :: [SolutionGrid -> Bool]
allRules = [ rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10
           , rule11, rule12, rule13, rule14, rule15
           ]

-- Generate all possible valid solution grids by permutation and filtering.
-- This uses a "generate and test" approach. It generates all permutations
-- for each category (color, resident, etc.) and combines them into potential
-- house grids. Each potential grid is then tested against all the rules.
-- Optimization: Rules 9 and 10 (fixed positions) are checked early to prune the search space.
possibleSolutions :: [SolutionGrid]
possibleSolutions =
  [ grid
  -- Generate all permutations for each category
  | pColor <- permutations [Red .. Blue]
  , pResident <- permutations [Englishman .. Japanese]
  -- Apply Rule 10 early: Check if Norwegian is at the first position in this resident permutation
  , residentAt pResident First == Norwegian
  , pPet <- permutations [Dog .. Zebra]
  , pDrink <- permutations [Coffee .. Water]
  -- Apply Rule 9 early: Check if Milk is at the third position in this drink permutation
  , drinkAt pDrink Third == Milk
  , pCigarette <- permutations [OldGold .. Parliaments]

  -- Construct the grid only *after* passing the early checks
  , let grid = [ House p co na pe dr ci
               | (p, co, na, pe, dr, ci) <- zip6 [First .. Fifth] pColor pResident pPet pDrink pCigarette
               ]
  -- Apply all remaining rules to the constructed grid
  , all ($ grid) remainingRules
  ]
  where
    -- Custom zip function for 6 lists
    zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
    zip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = (a,b,c,d,e,f) : zip6 as bs cs ds es fs
    zip6 _ _ _ _ _ _ = [] -- Base case: stop if any list is empty

    -- Helper to get the value at a specific position (index) from a permutation list
    valueAt :: [a] -> HousePos -> a
    valueAt xs p = xs !! fromEnum p -- Uses Enum instance of HousePos for indexing

    -- Specific versions of valueAt for readability
    residentAt :: [Resident] -> HousePos -> Resident
    residentAt = valueAt
    drinkAt :: [Drink] -> HousePos -> Drink
    drinkAt = valueAt

    -- Rules excluding those already applied (9 and 10)
    remainingRules :: [SolutionGrid -> Bool]
    remainingRules = [ rule2, rule3, rule4, rule5, rule6, rule7, rule8
                     , rule11, rule12, rule13, rule14, rule15
                     ]

-- The main function to solve the puzzle.
solve :: Solution
solve =
  -- Find the first (and presumably only) solution from the list of possibilities.
  case listToMaybe possibleSolutions of
    -- Exactly one solution found
    Just solutionGrid ->
      let -- Find the resident who drinks water
          waterDrinkerRes = maybe (error "Water drinker not found in the unique solution!") resident $
                            findHouse drink Water solutionGrid
          -- Find the resident who owns the zebra
          zebraOwnerRes   = maybe (error "Zebra owner not found in the unique solution!") resident $
                            findHouse pet Zebra solutionGrid
      -- Construct the final Solution object
      in Solution { waterDrinker = waterDrinkerRes, zebraOwner = zebraOwnerRes }
    -- No solution found
    Nothing -> error "No solution found that satisfies all constraints!"
    -- Note: The standard Zebra Puzzle has a unique solution. If multiple were found,
    -- listToMaybe would just pick the first one. An error for multiple solutions
    -- isn't strictly necessary based on the problem structure but could be added
    -- by checking `length possibleSolutions`.
