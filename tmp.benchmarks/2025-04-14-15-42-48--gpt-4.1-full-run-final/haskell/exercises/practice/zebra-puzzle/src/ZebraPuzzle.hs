module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, find)
import Data.Maybe (fromJust)

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

data House = House
  { resident :: Resident
  , color    :: Color
  , pet      :: Pet
  , drink    :: Drink
  , smoke    :: Smoke
  } deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Helper to get all possible orderings of 5 unique things
allOrderings :: (Enum a, Bounded a) => [[a]]
allOrderings = permutations [minBound .. maxBound]

-- The main solver
solve :: Solution
solve = 
  let
    -- Generate all possible arrangements for each attribute
    allResidents = allOrderings :: [[Resident]]
    allColors    = allOrderings :: [[Color]]
    allPets      = allOrderings :: [[Pet]]
    allDrinks    = allOrderings :: [[Drink]]
    allSmokes    = allOrderings :: [[Smoke]]

    -- Try all possible combinations, filter by constraints
    solutions = 
      [ [ House r c p d s | (r, c, p, d, s) <- zip5 rs cs ps ds ss ]
      | rs <- allResidents
      , cs <- allColors
      , ps <- allPets
      , ds <- allDrinks
      , ss <- allSmokes
      , isValid rs cs ps ds ss
      ]
    -- There is only one solution
    theHouses = head solutions

    waterGuy = resident $ fromJust $ find (\h -> drink h == Water) theHouses
    zebraGuy = resident $ fromJust $ find (\h -> pet h == Zebra) theHouses
  in
    Solution waterGuy zebraGuy

-- Helper: zip5 (not in Prelude for all GHC versions)
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a,b,c,d,e) : zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

-- All constraints from the puzzle
isValid :: [Resident] -> [Color] -> [Pet] -> [Drink] -> [Smoke] -> Bool
isValid rs cs ps ds ss =
  and
    [ -- 1. There are five houses. (implicit)
      length rs == 5
    , length cs == 5
    , length ps == 5
    , length ds == 5
    , length ss == 5

      -- 2. The Englishman lives in the red house.
    , sameIndex rs Englishman cs Red

      -- 3. The Spaniard owns the dog.
    , sameIndex rs Spaniard ps Dog

      -- 4. Coffee is drunk in the green house.
    , sameIndex ds Coffee cs Green

      -- 5. The Ukrainian drinks tea.
    , sameIndex rs Ukrainian ds Tea

      -- 6. The green house is immediately to the right of the ivory house.
    , rightOf cs Green cs Ivory

      -- 7. The Old Gold smoker owns snails.
    , sameIndex ss OldGold ps Snails

      -- 8. Kools are smoked in the yellow house.
    , sameIndex ss Kools cs Yellow

      -- 9. Milk is drunk in the middle house.
    , ds !! 2 == Milk

      -- 10. The Norwegian lives in the first house.
    , head rs == Norwegian

      -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    , nextTo ss Chesterfields ps Fox

      -- 12. Kools are smoked in the house next to the house where the horse is kept.
    , nextTo ss Kools ps Horse

      -- 13. The Lucky Strike smoker drinks orange juice.
    , sameIndex ss LuckyStrike ds OrangeJuice

      -- 14. The Japanese smokes Parliaments.
    , sameIndex rs Japanese ss Parliaments

      -- 15. The Norwegian lives next to the blue house.
    , nextTo rs Norwegian cs Blue
    ]

-- Helper: are x and y at the same index in their lists?
sameIndex :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
sameIndex as a bs b = case elemIndex a as of
  Just i  -> bs !! i == b
  Nothing -> False

-- Helper: is x immediately to the right of y in the list?
rightOf :: (Eq a) => [a] -> a -> [a] -> a -> Bool
rightOf as a bs b = case elemIndex b bs of
  Just i | i < 4 -> as !! (i+1) == a
  _ -> False

-- Helper: are x and y next to each other in their lists?
nextTo :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
nextTo as a bs b = case elemIndex a as of
  Just i -> (i > 0 && bs !! (i-1) == b) || (i < 4 && bs !! (i+1) == b)
  Nothing -> False

-- Helper: find the index of an element
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = go 0
  where
    go _ [] = Nothing
    go i (y:ys)
      | x == y    = Just i
      | otherwise = go (i+1) ys
