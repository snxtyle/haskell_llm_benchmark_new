module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, elemIndex)
import Data.Maybe (fromJust)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Define the other attributes
data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

-- House positions (0-indexed)
type Position = Int

-- A configuration is a permutation of each attribute
type Config a = [a]

-- Helper function to find the position of an element
positionOf :: Eq a => a -> [a] -> Position
positionOf x xs = fromJust $ elemIndex x xs

-- Helper function to check if two positions are adjacent
isNextTo :: Position -> Position -> Bool
isNextTo p1 p2 = abs (p1 - p2) == 1

-- Check if a configuration satisfies all constraints
isValidConfiguration :: Config Resident -> Config Color -> Config Pet -> Config Drink -> Config Smoke -> Bool
isValidConfiguration residents colors pets drinks smokes =
  -- 2. The Englishman lives in the red house
  positionOf Englishman residents == positionOf Red colors &&
  
  -- 3. The Spaniard owns the dog
  positionOf Spaniard residents == positionOf Dog pets &&
  
  -- 4. Coffee is drunk in the green house
  positionOf Coffee drinks == positionOf Green colors &&
  
  -- 5. The Ukrainian drinks tea
  positionOf Ukrainian residents == positionOf Tea drinks &&
  
  -- 6. The green house is immediately to the right of the ivory house
  positionOf Green colors == positionOf Ivory colors + 1 &&
  
  -- 7. The Old Gold smoker owns snails
  positionOf OldGold smokes == positionOf Snails pets &&
  
  -- 8. Kools are smoked in the yellow house
  positionOf Kools smokes == positionOf Yellow colors &&
  
  -- 9. Milk is drunk in the middle house
  positionOf Milk drinks == 2 &&
  
  -- 10. The Norwegian lives in the first house
  positionOf Norwegian residents == 0 &&
  
  -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox
  isNextTo (positionOf Chesterfields smokes) (positionOf Fox pets) &&
  
  -- 12. Kools are smoked in the house next to the house where the horse is kept
  isNextTo (positionOf Kools smokes) (positionOf Horse pets) &&
  
  -- 13. The Lucky Strike smoker drinks orange juice
  positionOf LuckyStrike smokes == positionOf OrangeJuice drinks &&
  
  -- 14. The Japanese smokes Parliaments
  positionOf Japanese residents == positionOf Parliaments smokes &&
  
  -- 15. The Norwegian lives next to the blue house
  isNextTo (positionOf Norwegian residents) (positionOf Blue colors)

-- Generate all possible configurations and find the valid one
findSolution :: Maybe (Config Resident, Config Color, Config Pet, Config Drink, Config Smoke)
findSolution = 
  let allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
      allColors = [Red, Green, Ivory, Yellow, Blue]
      allPets = [Dog, Snails, Fox, Horse, Zebra]
      allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]
      allSmokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
  in
    head' [(r, c, p, d, s) | 
           r <- permutations allResidents,
           c <- permutations allColors,
           p <- permutations allPets,
           d <- permutations allDrinks,
           s <- permutations allSmokes,
           isValidConfiguration r c p d s]
  where
    head' [] = Nothing
    head' (x:_) = Just x

solve :: Solution
solve = 
  case findSolution of
    Nothing -> error "No solution found"
    Just (residents, _, pets, drinks, _) ->
      let waterPos = positionOf Water drinks
          zebraPos = positionOf Zebra pets
          waterDrinker = residents !! waterPos
          zebraOwner = residents !! zebraPos
      in Solution waterDrinker zebraOwner
