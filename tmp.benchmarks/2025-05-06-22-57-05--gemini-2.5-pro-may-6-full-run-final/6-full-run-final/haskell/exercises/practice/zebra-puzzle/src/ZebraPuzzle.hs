module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, findIndex)
import Control.Monad (guard)

-- Provided data types
data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded) -- Added Enum, Bounded

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- New data types for puzzle elements
data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show, Enum, Bounded)

data Beverage = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show, Enum, Bounded)

data Cigarette = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show, Enum, Bounded)

-- Type alias for a tuple representing all properties of a single house.
-- The order is Color, Resident (Nationality), Pet, Beverage, Cigarette.
type HouseProperties = (Color, Resident, Pet, Beverage, Cigarette)

-- Helper function to zip five lists. Standard libraries usually go up to zip4 or zipWith4.
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e) : zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

solve :: Solution
solve = head $ do -- The puzzle is known to have a unique solution.

  -- Generate all possible assignments of properties to the 5 houses.
  -- Each list (e.g., colorsP) is a permutation of all possible values for that category.
  -- The i-th element in such a list corresponds to the property of house (i+1).
  -- For example, colorsP !! 0 is the color of the first house.
  colorsP     <- permutations [minBound .. maxBound :: Color]
  residentsP  <- permutations [minBound .. maxBound :: Resident]
  petsP       <- permutations [minBound .. maxBound :: Pet]
  beveragesP  <- permutations [minBound .. maxBound :: Beverage]
  cigarettesP <- permutations [minBound .. maxBound :: Cigarette]

  -- Combine these permutations into a list where each element represents a house
  -- and contains all its properties. houseCombinedProperties !! i are properties of house (i+1).
  let houseCombinedProperties :: [HouseProperties]
      houseCombinedProperties = zip5 colorsP residentsP petsP beveragesP cigarettesP

  -- Apply constraints (rules of the puzzle) using 'guard'.
  -- If a guard fails, this particular combination of permutations is discarded.

  -- Rule 10: The Norwegian lives in the first house (index 0).
  guard (residentsP !! 0 == Norwegian)

  -- Rule 9: Milk is drunk in the middle house (house 3, index 2).
  guard (beveragesP !! 2 == Milk)

  -- Rule 15: The Norwegian lives next to the blue house.
  -- Since the Norwegian is in house 1 (index 0, from Rule 10),
  -- house 2 (index 1) must be blue.
  guard (colorsP !! 1 == Blue)

  -- Constraints linking properties within the same house:
  -- Rule 2: The Englishman lives in the red house.
  guard $ any (\(c,n,_,_,_) -> n == Englishman && c == Red) houseCombinedProperties
  -- Rule 3: The Spaniard owns the dog.
  guard $ any (\(_,n,p,_,_) -> n == Spaniard && p == Dog) houseCombinedProperties
  -- Rule 4: Coffee is drunk in the green house.
  guard $ any (\(c,_,_,b,_) -> b == Coffee && c == Green) houseCombinedProperties
  -- Rule 5: The Ukrainian drinks tea.
  guard $ any (\(_,n,_,b,_) -> n == Ukrainian && b == Tea) houseCombinedProperties
  -- Rule 7: The Old Gold smoker owns snails.
  guard $ any (\(_,_,p,_,cig) -> cig == OldGold && p == Snails) houseCombinedProperties
  -- Rule 8: Kools are smoked in the yellow house.
  guard $ any (\(c,_,_,_,cig) -> cig == Kools && c == Yellow) houseCombinedProperties
  -- Rule 13: The Lucky Strike smoker drinks orange juice.
  guard $ any (\(_,_,_,b,cig) -> cig == LuckyStrike && b == OrangeJuice) houseCombinedProperties
  -- Rule 14: The Japanese smokes Parliaments.
  guard $ any (\(_,n,_,_,cig) -> n == Japanese && cig == Parliaments) houseCombinedProperties

  -- Constraints involving relative house positions:

  -- Rule 6: The green house is immediately to the right of the ivory house.
  -- (ivory_house_idx, green_house_idx) must be (i, i+1) for some i.
  case findIndex (\(c,_,_,_,_) -> c == Green) houseCombinedProperties of
    Just greenHouseIdx ->
      -- Green house cannot be the first house (index 0) if it's to the right of another.
      guard (greenHouseIdx > 0 && (colorsP !! (greenHouseIdx - 1) == Ivory))
    Nothing -> guard False -- Should not happen as Green is in colorsP.

  -- Rule 11: The man who smokes Chesterfields lives in the house next to the man with the fox.
  case findIndex (\(_,_,_,_,cig) -> cig == Chesterfields) houseCombinedProperties of
    Just cfSmokerIdx ->
      let isNextToFox = (cfSmokerIdx > 0 && (petsP !! (cfSmokerIdx - 1) == Fox)) ||
                        (cfSmokerIdx < 4 && (petsP !! (cfSmokerIdx + 1) == Fox))
      in guard isNextToFox
    Nothing -> guard False -- Chesterfields smoker must exist.

  -- Rule 12: Kools are smoked in the house next to the house where the horse is kept.
  case findIndex (\(_,_,_,_,cig) -> cig == Kools) houseCombinedProperties of
    Just koolsSmokerIdx ->
      let isNextToHorse = (koolsSmokerIdx > 0 && (petsP !! (koolsSmokerIdx - 1) == Horse)) ||
                          (koolsSmokerIdx < 4 && (petsP !! (koolsSmokerIdx + 1) == Horse))
      in guard isNextToHorse
    Nothing -> guard False -- Kools smoker must exist.

  -- If all guards have passed, this combination of permutations is a valid solution.
  -- Now, find the resident who drinks water and the resident who owns the zebra.

  let findResidentByPredicate :: (HouseProperties -> Bool) -> String -> Resident
      findResidentByPredicate pred errorMsg =
        case findIndex pred houseCombinedProperties of
          Just idx -> residentsP !! idx
          Nothing  -> error errorMsg -- Should not happen in a valid solution.

  let waterDrinkerResident = findResidentByPredicate (\(_,_,_,b,_) -> b == Water) "Water drinker not found"
  let zebraOwnerResident   = findResidentByPredicate (\(_,_,p,_,_) -> p == Zebra) "Zebra owner not found"

  -- Return the solution.
  return Solution { waterDrinker = waterDrinkerResident, zebraOwner = zebraOwnerResident }

