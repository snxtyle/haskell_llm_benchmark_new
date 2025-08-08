module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, find, zipWith5)
import Data.Maybe (fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

data House = House
  { resident :: Resident
  , color :: Color
  , pet :: Pet
  , drink :: Drink
  , cigarette :: Cigarette
  } deriving (Eq, Show)

type Street = [House]

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper function to get all permutations of an enum type
allPerms :: (Enum a, Bounded a) => [[a]]
allPerms = permutations [minBound .. maxBound]

-- Helper function to find house index by attribute
findIndex :: (House -> Bool) -> Street -> Maybe Int
findIndex pred street = fmap fst $ find (pred . snd) $ zip [0..] street

-- Helper function to check if two houses are adjacent
areAdjacent :: (House -> Bool) -> (House -> Bool) -> Street -> Bool
areAdjacent pred1 pred2 street = 
  case (findIndex pred1 street, findIndex pred2 street) of
    (Just i1, Just i2) -> abs (i1 - i2) == 1
    _ -> False

-- Check if the green house is immediately to the right of ivory
greenRightOfIvory :: Street -> Bool
greenRightOfIvory street = 
  case (findIndex ((== Ivory) . color) street, findIndex ((== Green) . color) street) of
    (Just i1, Just i2) -> i2 == i1 + 1
    _ -> False

-- Check all constraints
checkConstraints :: Street -> Bool
checkConstraints street = all ($ street)
  [ -- 2. The Englishman lives in the red house
    \s -> any (\h -> resident h == Englishman && color h == Red) s
    
    -- 3. The Spaniard owns the dog
  , \s -> any (\h -> resident h == Spaniard && pet h == Dog) s
    
    -- 4. Coffee is drunk in the green house
  , \s -> any (\h -> drink h == Coffee && color h == Green) s
    
    -- 5. The Ukrainian drinks tea
  , \s -> any (\h -> resident h == Ukrainian && drink h == Tea) s
    
    -- 6. The green house is immediately to the right of the ivory house
  , greenRightOfIvory
    
    -- 7. The Old Gold smoker owns snails
  , \s -> any (\h -> cigarette h == OldGold && pet h == Snails) s
    
    -- 8. Kools are smoked in the yellow house
  , \s -> any (\h -> cigarette h == Kools && color h == Yellow) s
    
    -- 9. Milk is drunk in the middle house (house 3, index 2)
  , \s -> drink (s !! 2) == Milk
    
    -- 10. The Norwegian lives in the first house (index 0)
  , \s -> resident (head s) == Norwegian
    
    -- 11. The man who smokes Chesterfields lives next to the man with the fox
  , areAdjacent ((== Chesterfields) . cigarette) ((== Fox) . pet)
    
    -- 12. Kools are smoked in the house next to the house where the horse is kept
  , areAdjacent ((== Kools) . cigarette) ((== Horse) . pet)
    
    -- 13. The Lucky Strike smoker drinks orange juice
  , \s -> any (\h -> cigarette h == LuckyStrike && drink h == OrangeJuice) s
    
    -- 14. The Japanese smokes Parliaments
  , \s -> any (\h -> resident h == Japanese && cigarette h == Parliaments) s
    
    -- 15. The Norwegian lives next to the blue house
  , areAdjacent ((== Norwegian) . resident) ((== Blue) . color)
  ]

-- Generate all possible street configurations
generateStreets :: [Street]
generateStreets = do
  residents <- allPerms
  colors <- allPerms
  pets <- allPerms
  drinks <- allPerms
  cigarettes <- allPerms
  return $ zipWith5 House residents colors pets drinks cigarettes

-- Find the solution
findSolution :: Street
findSolution = fromJust $ find checkConstraints generateStreets

solve :: Solution
solve = 
  let street = findSolution
      waterHouse = fromJust $ find ((== Water) . drink) street
      zebraHouse = fromJust $ find ((== Zebra) . pet) street
  in Solution (resident waterHouse) (resident zebraHouse)
