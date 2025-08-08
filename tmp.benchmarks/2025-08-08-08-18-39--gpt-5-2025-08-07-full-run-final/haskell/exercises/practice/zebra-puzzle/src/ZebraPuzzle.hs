module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, elemIndex)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Internal enums for attributes in the puzzle
data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)
data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)
data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

solve :: Solution
solve = head solutions
  where
    solutions =
      [ Solution
          { waterDrinker = residentAt waterPos nats
          , zebraOwner   = residentAt zebraPos nats
          }
      | cols <- colorOrders
      , nats <- nationalityOrders cols
      , drs  <- drinkOrders cols nats
      , sms  <- smokeOrders cols nats drs
      , pts  <- petOrders nats sms
      , let waterPos = posOf Water ptsDrinks drs
      , let zebraPos = posOf Zebra ptsPets pts
      ]
      where
        -- Helper to map position to resident
        residentAt :: Int -> [Resident] -> Resident
        residentAt p rs = rs !! (p - 1)

        -- Positions are 1..5
        positions :: [Int]
        positions = [1..5]

        adjacent :: Int -> Int -> Bool
        adjacent i j = abs (i - j) == 1

        -- Generic position finder for a value within a positional list
        posOf :: Eq a => a -> ([a] -> [a]) -> [a] -> Int
        posOf x _ xs = case elemIndex x xs of
          Just i  -> i + 1
          Nothing -> error "Value not found in list"

        -- Identity accessors to make types clear at call sites
        ptsDrinks :: [Drink] -> [Drink]
        ptsDrinks = id

        ptsPets :: [Pet] -> [Pet]
        ptsPets = id

        -- All possible values
        allResidents :: [Resident]
        allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]

        allColors :: [Color]
        allColors = [Red, Green, Ivory, Yellow, Blue]

        allDrinks :: [Drink]
        allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]

        allSmokes :: [Smoke]
        allSmokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]

        allPets :: [Pet]
        allPets = [Dog, Snails, Fox, Horse, Zebra]

        -- Orders that satisfy "Green immediately to the right of Ivory"
        colorOrders :: [[Color]]
        colorOrders =
          [ cols
          | cols <- permutations allColors
          , pos Green cols == pos Ivory cols + 1
          ]
          where
            pos :: Color -> [Color] -> Int
            pos c xs = case elemIndex c xs of
              Just i  -> i + 1
              Nothing -> error "Color not found"

        -- Residents with:
        -- - Norwegian in the first house
        -- - Englishman in the red house
        -- - Norwegian lives next to the blue house
        nationalityOrders :: [Color] -> [[Resident]]
        nationalityOrders cols =
          [ nats
          | nats <- permutations allResidents
          , head nats == Norwegian
          , pos Englishman nats == posC Red
          , adjacent (pos Norwegian nats) (posC Blue)
          ]
          where
            posC :: Color -> Int
            posC c = case elemIndex c cols of
              Just i  -> i + 1
              Nothing -> error "Color not found in cols"
            pos :: Resident -> [Resident] -> Int
            pos r xs = case elemIndex r xs of
              Just i  -> i + 1
              Nothing -> error "Resident not found"

        -- Drinks with:
        -- - Milk in the middle house
        -- - Coffee in the green house
        -- - Ukrainian drinks tea
        drinkOrders :: [Color] -> [Resident] -> [[Drink]]
        drinkOrders cols nats =
          [ drs
          | drs <- permutations allDrinks
          , drs !! 2 == Milk
          , pos Coffee drs == posC Green
          , pos Tea drs == posR Ukrainian
          ]
          where
            pos :: Drink -> [Drink] -> Int
            pos d xs = case elemIndex d xs of
              Just i  -> i + 1
              Nothing -> error "Drink not found"
            posC :: Color -> Int
            posC c = case elemIndex c cols of
              Just i  -> i + 1
              Nothing -> error "Color not found in cols"
            posR :: Resident -> Int
            posR r = case elemIndex r nats of
              Just i  -> i + 1
              Nothing -> error "Resident not found in nats"

        -- Smokes with:
        -- - Kools in the yellow house
        -- - Lucky Strike smoker drinks orange juice
        -- - Japanese smokes Parliaments
        smokeOrders :: [Color] -> [Resident] -> [Drink] -> [[Smoke]]
        smokeOrders cols nats drs =
          [ sms
          | sms <- permutations allSmokes
          , pos Kools sms == posC Yellow
          , pos LuckyStrike sms == posD OrangeJuice
          , pos Parliaments sms == posR Japanese
          ]
          where
            pos :: Smoke -> [Smoke] -> Int
            pos s xs = case elemIndex s xs of
              Just i  -> i + 1
              Nothing -> error "Smoke not found"
            posC :: Color -> Int
            posC c = case elemIndex c cols of
              Just i  -> i + 1
              Nothing -> error "Color not found in cols"
            posD :: Drink -> Int
            posD d = case elemIndex d drs of
              Just i  -> i + 1
              Nothing -> error "Drink not found in drs"
            posR :: Resident -> Int
            posR r = case elemIndex r nats of
              Just i  -> i + 1
              Nothing -> error "Resident not found in nats"

        -- Pets with:
        -- - Spaniard owns the dog
        -- - Old Gold smoker owns snails
        -- - Chesterfields smoker lives next to the fox
        -- - Kools smoker lives next to the horse
        petOrders :: [Resident] -> [Smoke] -> [[Pet]]
        petOrders nats sms =
          [ pts
          | pts <- permutations allPets
          , pos Dog pts == posR Spaniard
          , pos Snails pts == posS OldGold
          , adjacent (posS Chesterfields) (pos Fox pts)
          , adjacent (posS Kools) (pos Horse pts)
          ]
          where
            pos :: Pet -> [Pet] -> Int
            pos p xs = case elemIndex p xs of
              Just i  -> i + 1
              Nothing -> error "Pet not found"
            posR :: Resident -> Int
            posR r = case elemIndex r nats of
              Just i  -> i + 1
              Nothing -> error "Resident not found in nats"
            posS :: Smoke -> Int
            posS s = case elemIndex s sms of
              Just i  -> i + 1
              Nothing -> error "Smoke not found in smokes"
