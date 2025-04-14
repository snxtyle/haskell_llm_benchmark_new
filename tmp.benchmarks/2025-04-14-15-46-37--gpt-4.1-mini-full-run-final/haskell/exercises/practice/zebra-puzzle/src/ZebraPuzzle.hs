module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, find, elemIndex, findIndex)
import Data.Maybe (fromJust)
import Prelude (zip5)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

data House = House { resident :: Resident
                   , color :: Color
                   , pet :: Pet
                   , drink :: Drink
                   , smoke :: Smoke
                   } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper to check if house a is immediately to the right of house b
isRightOf :: Eq a => [a] -> a -> a -> Bool
isRightOf xs a b = case (elemIndex a xs, elemIndex b xs) of
  (Just i, Just j) -> i == j + 1
  _ -> False

-- Helper to check if two houses are next to each other
isNextTo :: Eq a => [a] -> a -> a -> Bool
isNextTo xs a b = case (elemIndex a xs, elemIndex b xs) of
  (Just i, Just j) -> abs (i - j) == 1
  _ -> False

solve :: Solution
solve = findSolution $ fromJust $ find isValidSolution allSolutions
  where
    allColors = [Red, Green, Ivory, Yellow, Blue]
    allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
    allPets = [Dog, Snails, Fox, Horse, Zebra]
    allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]
    allSmokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]

    -- All permutations of each attribute
    colorPerms = permutations allColors
    residentPerms = permutations allResidents
    petPerms = permutations allPets
    drinkPerms = permutations allDrinks
    smokePerms = permutations allSmokes

    -- Generate all possible houses lists
    allSolutions = do
      colors <- colorPerms
      residents <- residentPerms
      pets <- petPerms
      drinks <- drinkPerms
      smokes <- smokePerms

      let houses = [ House r c p d s | (r,c,p,d,s) <- zip5 residents colors pets drinks smokes ]

      -- Check uniqueness of attributes is guaranteed by permutations

      -- Check constraints
      if isValid houses then return houses else []

    isValid :: [House] -> Bool
    isValid houses =
      -- 1. There are five houses. (guaranteed)
      length houses == 5 &&

      -- 2. The Englishman lives in the red house.
      any (\h -> resident h == Englishman && color h == Red) houses &&

      -- 3. The Spaniard owns the dog.
      any (\h -> resident h == Spaniard && pet h == Dog) houses &&

      -- 4. Coffee is drunk in the green house.
      any (\h -> drink h == Coffee && color h == Green) houses &&

      -- 5. The Ukrainian drinks tea.
      any (\h -> resident h == Ukrainian && drink h == Tea) houses &&

      -- 6. The green house is immediately to the right of the ivory house.
      isRightOf (map color houses) Green Ivory &&

      -- 7. The Old Gold smoker owns snails.
      any (\h -> smoke h == OldGold && pet h == Snails) houses &&

      -- 8. Kools are smoked in the yellow house.
      any (\h -> smoke h == Kools && color h == Yellow) houses &&

      -- 9. Milk is drunk in the middle house.
      drink (houses !! 2) == Milk &&

      -- 10. The Norwegian lives in the first house.
      resident (head houses) == Norwegian &&

      -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
      let chesterfieldIndex = fromJust $ findIndex (\h -> smoke h == Chesterfields) houses
          foxIndex = fromJust $ findIndex (\h -> pet h == Fox) houses
      in abs (chesterfieldIndex - foxIndex) == 1 &&

      -- 12. Kools are smoked in the house next to the house where the horse is kept.
      let koolsIndex = fromJust $ findIndex (\h -> smoke h == Kools) houses
          horseIndex = fromJust $ findIndex (\h -> pet h == Horse) houses
      in abs (koolsIndex - horseIndex) == 1 &&

      -- 13. The Lucky Strike smoker drinks orange juice.
      any (\h -> smoke h == LuckyStrike && drink h == OrangeJuice) houses &&

      -- 14. The Japanese smokes Parliaments.
      any (\h -> resident h == Japanese && smoke h == Parliaments) houses &&

      -- 15. The Norwegian lives next to the blue house.
      let norwegianIndex = fromJust $ findIndex (\h -> resident h == Norwegian) houses
          blueIndex = fromJust $ findIndex (\h -> color h == Blue) houses
      in abs (norwegianIndex - blueIndex) == 1

    -- Extract the solution from the valid houses
    findSolution :: [House] -> Solution
    findSolution houses =
      let waterDrinkerResident = resident $ fromJust $ find (\h -> drink h == Water) houses
          zebraOwnerResident = resident $ fromJust $ find (\h -> pet h == Zebra) houses
      in Solution waterDrinkerResident zebraOwnerResident

    isValidSolution :: [House] -> Bool
    isValidSolution = isValid
