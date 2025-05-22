module ZebraPuzzle (Resident(..), Solution(..), solve) where

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

data House = House { position :: Int
                   , color :: Color
                   , resident :: Resident
                   , pet :: Pet
                   , drink :: Drink
                   , cigarette :: Cigarette
                   } deriving (Eq, Show)

solve :: Solution
solve = 
  let solutions = [Solution (resident (houses !! waterPos)) (resident (houses !! zebraPos))
                  | houses <- validSolutions
                  , let waterPos = findDrinkPosition Water houses
                  , let zebraPos = findPetPosition Zebra houses
                  ]
  in head solutions

findDrinkPosition :: Drink -> [House] -> Int
findDrinkPosition target houses = 
  case [i | (i, house) <- zip [0..] houses, drink house == target] of
    (pos:_) -> pos
    [] -> error "Drink not found"

findPetPosition :: Pet -> [House] -> Int
findPetPosition target houses = 
  case [i | (i, house) <- zip [0..] houses, pet house == target] of
    (pos:_) -> pos
    [] -> error "Pet not found"

validSolutions :: [[House]]
validSolutions = filter satisfiesAllConstraints allPossibleSolutions

allPossibleSolutions :: [[House]]
allPossibleSolutions = 
  [ [House 1 c1 r1 p1 d1 s1, House 2 c2 r2 p2 d2 s2, House 3 c3 r3 p3 d3 s3, House 4 c4 r4 p4 d4 s4, House 5 c5 r5 p5 d5 s5]
  | [c1,c2,c3,c4,c5] <- permutations [Red, Green, Ivory, Yellow, Blue]
  , [r1,r2,r3,r4,r5] <- permutations [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
  , [p1,p2,p3,p4,p5] <- permutations [Dog, Snails, Fox, Horse, Zebra]
  , [d1,d2,d3,d4,d5] <- permutations [Coffee, Tea, Milk, OrangeJuice, Water]
  , [s1,s2,s3,s4,s5] <- permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  ]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:ys | y <- xs, ys <- permutations (filter (/= y) xs)]

satisfiesAllConstraints :: [House] -> Bool
satisfiesAllConstraints houses = all ($ houses) constraints

constraints :: [[House] -> Bool]
constraints = 
  [ constraint2   -- Englishman lives in red house
  , constraint3   -- Spaniard owns dog
  , constraint4   -- Coffee drunk in green house
  , constraint5   -- Ukrainian drinks tea
  , constraint6   -- Green house immediately right of ivory house
  , constraint7   -- Old Gold smoker owns snails
  , constraint8   -- Kools smoked in yellow house
  , constraint9   -- Milk drunk in middle house
  , constraint10  -- Norwegian lives in first house
  , constraint11  -- Chesterfields smoker next to fox owner
  , constraint12  -- Kools smoker next to horse owner
  , constraint13  -- Lucky Strike smoker drinks orange juice
  , constraint14  -- Japanese smokes Parliaments
  , constraint15  -- Norwegian next to blue house
  ]

constraint2 :: [House] -> Bool
constraint2 houses = any (\h -> resident h == Englishman && color h == Red) houses

constraint3 :: [House] -> Bool
constraint3 houses = any (\h -> resident h == Spaniard && pet h == Dog) houses

constraint4 :: [House] -> Bool
constraint4 houses = any (\h -> drink h == Coffee && color h == Green) houses

constraint5 :: [House] -> Bool
constraint5 houses = any (\h -> resident h == Ukrainian && drink h == Tea) houses

constraint6 :: [House] -> Bool
constraint6 houses = 
  any (\(h1, h2) -> color h1 == Ivory && color h2 == Green && position h2 == position h1 + 1)
      [(h1, h2) | h1 <- houses, h2 <- houses]

constraint7 :: [House] -> Bool
constraint7 houses = any (\h -> cigarette h == OldGold && pet h == Snails) houses

constraint8 :: [House] -> Bool
constraint8 houses = any (\h -> cigarette h == Kools && color h == Yellow) houses

constraint9 :: [House] -> Bool
constraint9 houses = any (\h -> drink h == Milk && position h == 3) houses

constraint10 :: [House] -> Bool
constraint10 houses = any (\h -> resident h == Norwegian && position h == 1) houses

constraint11 :: [House] -> Bool
constraint11 houses = 
  any (\(h1, h2) -> cigarette h1 == Chesterfields && pet h2 == Fox && abs (position h1 - position h2) == 1)
      [(h1, h2) | h1 <- houses, h2 <- houses]

constraint12 :: [House] -> Bool
constraint12 houses = 
  any (\(h1, h2) -> cigarette h1 == Kools && pet h2 == Horse && abs (position h1 - position h2) == 1)
      [(h1, h2) | h1 <- houses, h2 <- houses]

constraint13 :: [House] -> Bool
constraint13 houses = any (\h -> cigarette h == LuckyStrike && drink h == OrangeJuice) houses

constraint14 :: [House] -> Bool
constraint14 houses = any (\h -> resident h == Japanese && cigarette h == Parliaments) houses

constraint15 :: [House] -> Bool
constraint15 houses = 
  any (\(h1, h2) -> resident h1 == Norwegian && color h2 == Blue && abs (position h1 - position h2) == 1)
      [(h1, h2) | h1 <- houses, h2 <- houses]
