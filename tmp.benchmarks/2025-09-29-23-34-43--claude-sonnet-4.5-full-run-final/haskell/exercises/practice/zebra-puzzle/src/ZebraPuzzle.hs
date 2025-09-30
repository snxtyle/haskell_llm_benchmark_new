module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, (\\))

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

type House = (Resident, Color, Pet, Drink, Cigarette)
type Street = [House]

solve :: Solution
solve = 
  case findSolution of
    Just street -> Solution { waterDrinker = findDrinker Water street
                            , zebraOwner = findOwner Zebra street
                            }
    Nothing -> error "No solution found"

findSolution :: Maybe Street
findSolution = 
  let allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
      allColors = [Red, Green, Ivory, Yellow, Blue]
      allPets = [Dog, Snails, Fox, Horse, Zebra]
      allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]
      allCigarettes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  in findStreet allResidents allColors allPets allDrinks allCigarettes

findStreet :: [Resident] -> [Color] -> [Pet] -> [Drink] -> [Cigarette] -> Maybe Street
findStreet residents colors pets drinks cigarettes = 
  let solutions = do
        -- Constraint 10: Norwegian in first house
        let r1 = Norwegian
        let remainingResidents1 = residents \\ [r1]
        
        -- Constraint 15: Norwegian next to blue house (so blue is in position 2)
        let c2 = Blue
        c1 <- colors \\ [c2]
        let remainingColors1 = colors \\ [c1, c2]
        
        -- Constraint 9: Milk in middle house (position 3)
        let d3 = Milk
        let remainingDrinks3 = drinks \\ [d3]
        
        -- Try to build houses incrementally
        p1 <- pets
        d1 <- drinks \\ [d3]
        cig1 <- cigarettes
        
        let house1 = (r1, c1, p1, d1, cig1)
        
        -- Build remaining houses
        r2 <- remainingResidents1
        let remainingResidents2 = remainingResidents1 \\ [r2]
        p2 <- pets \\ [p1]
        d2 <- drinks \\ [d1, d3]
        cig2 <- cigarettes \\ [cig1]
        
        let house2 = (r2, c2, p2, d2, cig2)
        
        r3 <- remainingResidents2
        let remainingResidents3 = remainingResidents2 \\ [r3]
        c3 <- remainingColors1
        let remainingColors2 = remainingColors1 \\ [c3]
        p3 <- pets \\ [p1, p2]
        cig3 <- cigarettes \\ [cig1, cig2]
        
        let house3 = (r3, c3, p3, d3, cig3)
        
        r4 <- remainingResidents3
        let r5 = head (remainingResidents3 \\ [r4])
        c4 <- remainingColors2
        let c5 = head (remainingColors2 \\ [c4])
        p4 <- pets \\ [p1, p2, p3]
        let p5 = head (pets \\ [p1, p2, p3, p4])
        d4 <- remainingDrinks3
        let d5 = head (remainingDrinks3 \\ [d4])
        cig4 <- cigarettes \\ [cig1, cig2, cig3]
        let cig5 = head (cigarettes \\ [cig1, cig2, cig3, cig4])
        
        let house4 = (r4, c4, p4, d4, cig4)
        let house5 = (r5, c5, p5, d5, cig5)
        
        let street = [house1, house2, house3, house4, house5]
        
        if satisfiesAllConstraints street
          then return street
          else []
  in case solutions of
       (s:_) -> Just s
       [] -> Nothing

satisfiesAllConstraints :: Street -> Bool
satisfiesAllConstraints street =
  constraint2 street &&
  constraint3 street &&
  constraint4 street &&
  constraint5 street &&
  constraint6 street &&
  constraint7 street &&
  constraint8 street &&
  constraint11 street &&
  constraint12 street &&
  constraint13 street &&
  constraint14 street

-- Helper functions
hasAttribute :: (House -> Bool) -> Street -> Bool
hasAttribute predicate = any predicate

getPosition :: (House -> Bool) -> Street -> Maybe Int
getPosition predicate street = 
  case filter (\(_, h) -> predicate h) (zip [0..] street) of
    ((pos, _):_) -> Just pos
    [] -> Nothing

nextTo :: Int -> Int -> Bool
nextTo a b = abs (a - b) == 1

rightOf :: Int -> Int -> Bool
rightOf a b = a == b + 1

-- Constraint 2: The Englishman lives in the red house
constraint2 :: Street -> Bool
constraint2 = hasAttribute (\(r, c, _, _, _) -> r == Englishman && c == Red)

-- Constraint 3: The Spaniard owns the dog
constraint3 :: Street -> Bool
constraint3 = hasAttribute (\(r, _, p, _, _) -> r == Spaniard && p == Dog)

-- Constraint 4: Coffee is drunk in the green house
constraint4 :: Street -> Bool
constraint4 = hasAttribute (\(_, c, _, d, _) -> c == Green && d == Coffee)

-- Constraint 5: The Ukrainian drinks tea
constraint5 :: Street -> Bool
constraint5 = hasAttribute (\(r, _, _, d, _) -> r == Ukrainian && d == Tea)

-- Constraint 6: The green house is immediately to the right of the ivory house
constraint6 :: Street -> Bool
constraint6 street =
  case (getPosition (\(_, c, _, _, _) -> c == Green) street,
        getPosition (\(_, c, _, _, _) -> c == Ivory) street) of
    (Just g, Just i) -> rightOf g i
    _ -> False

-- Constraint 7: The Old Gold smoker owns snails
constraint7 :: Street -> Bool
constraint7 = hasAttribute (\(_, _, p, _, cig) -> cig == OldGold && p == Snails)

-- Constraint 8: Kools are smoked in the yellow house
constraint8 :: Street -> Bool
constraint8 = hasAttribute (\(_, c, _, _, cig) -> c == Yellow && cig == Kools)

-- Constraint 11: The man who smokes Chesterfields lives next to the man with the fox
constraint11 :: Street -> Bool
constraint11 street =
  case (getPosition (\(_, _, _, _, cig) -> cig == Chesterfields) street,
        getPosition (\(_, _, p, _, _) -> p == Fox) street) of
    (Just c, Just f) -> nextTo c f
    _ -> False

-- Constraint 12: Kools are smoked in the house next to the house where the horse is kept
constraint12 :: Street -> Bool
constraint12 street =
  case (getPosition (\(_, _, _, _, cig) -> cig == Kools) street,
        getPosition (\(_, _, p, _, _) -> p == Horse) street) of
    (Just k, Just h) -> nextTo k h
    _ -> False

-- Constraint 13: The Lucky Strike smoker drinks orange juice
constraint13 :: Street -> Bool
constraint13 = hasAttribute (\(_, _, _, d, cig) -> cig == LuckyStrike && d == OrangeJuice)

-- Constraint 14: The Japanese smokes Parliaments
constraint14 :: Street -> Bool
constraint14 = hasAttribute (\(r, _, _, _, cig) -> r == Japanese && cig == Parliaments)

-- Find who drinks a specific drink
findDrinker :: Drink -> Street -> Resident
findDrinker drink street =
  case filter (\(_, _, _, d, _) -> d == drink) street of
    ((r, _, _, _, _):_) -> r
    [] -> error "Drink not found"

-- Find who owns a specific pet
findOwner :: Pet -> Street -> Resident
findOwner pet street =
  case filter (\(_, _, p, _, _) -> p == pet) street of
    ((r, _, _, _, _):_) -> r
    [] -> error "Pet not found"
