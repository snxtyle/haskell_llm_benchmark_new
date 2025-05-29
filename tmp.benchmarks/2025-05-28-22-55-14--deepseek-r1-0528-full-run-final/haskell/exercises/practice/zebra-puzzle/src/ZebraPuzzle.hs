module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show)
data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Eq, Show)
data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = Solution waterDrinker zebraOwner
  where
    (waterDrinker, zebraOwner) = head solutions
    solutions = [ (waterDrinkerRes, zebraOwnerRes)
                | c0 : c2 : c3 : c4 <- permutations [Red, Green, Ivory, Yellow]
                , let colors = [c0, Blue, c2, c3, c4]
                , (c2 == Ivory && c3 == Green) || (c3 == Ivory && c4 == Green)  -- Constraint 6
                , n1:n2:n3:n4 <- permutations [Englishman, Spaniard, Ukrainian, Japanese]
                , let nationalities = [Norwegian, n1, n2, n3, n4]
                , d0:d1:d3:d4 <- permutations [Coffee, Tea, OrangeJuice, Water]
                , let drinks = [d0, d1, Milk, d3, d4]  -- Constraint 9
                , smokes <- permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
                , pets <- permutations [Dog, Snails, Fox, Horse, Zebra]
                , let street = zip5 colors nationalities drinks smokes pets
                , any (\(col, nat, _, _, _) -> nat == Englishman && col == Red) street  -- Constraint 2
                , any (\(_, nat, _, _, pet) -> nat == Spaniard && pet == Dog) street  -- Constraint 3
                , any (\(col, _, drink, _, _) -> drink == Coffee && col == Green) street  -- Constraint 4
                , any (\(_, nat, drink, _, _) -> nat == Ukrainian && drink == Tea) street  -- Constraint 5
                , any (\(_, _, _, smoke, pet) -> smoke == OldGold && pet == Snails) street  -- Constraint 7
                , any (\(col, _, _, smoke, _) -> smoke == Kools && col == Yellow) street  -- Constraint 8
                , any (\i -> 
                        let (_, _, _, s, _) = street !! i
                        in s == Chesterfields &&
                           ( (i > 0 && (let (_, _, _, _, p) = street !! (i-1) in p == Fox)) ||
                             (i < 4 && (let (_, _, _, _, p) = street !! (i+1) in p == Fox)) 
                           )
                     ) [0..4]  -- Constraint 11
                , any (\i -> 
                        let (_, _, _, s, _) = street !! i
                        in s == Kools &&
                           ( (i > 0 && (let (_, _, _, _, p) = street !! (i-1) in p == Horse)) ||
                             (i < 4 && (let (_, _, _, _, p) = street !! (i+1) in p == Horse)) 
                           )
                     ) [0..4]  -- Constraint 12
                , any (\(_, _, drink, smoke, _) -> smoke == LuckyStrike && drink == OrangeJuice) street  -- Constraint 13
                , any (\(_, nat, _, smoke, _) -> nat == Japanese && smoke == Parliaments) street  -- Constraint 14
                , let waterDrinkerRes = head [ nat | (_, nat, drink, _, _) <- street, drink == Water ]
                      zebraOwnerRes = head [ nat | (_, nat, _, _, pet) <- street, pet == Zebra ]
                ]
