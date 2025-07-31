module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (permutations, elemIndex)
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

-- Solution type expected by tests
data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner   :: Resident
  } deriving (Eq, Show)

-- Represent houses by position 1..5 (left to right), with 3 being middle.
positions :: [Int]
positions = [1..5]

middlePos :: Int
middlePos = 3

firstPos :: Int
firstPos = 1

rightOf :: Int -> Int -> Bool
rightOf a b = a == b + 1

nextTo :: Int -> Int -> Bool
nextTo a b = abs (a - b) == 1

-- A complete assignment maps each attribute value to a position.
-- We'll construct consistent assignments via permutations with pruning.
solve :: Solution
solve = Solution { waterDrinker = toResident waterPos
                 , zebraOwner   = toResident zebraPos
                 }
  where
    -- Start with residents positions with known constraint: Norwegian at 1
    residentsPerms =
      [ rs
      | rs <- permutations positions
      , pos Norwegian rs == firstPos
      ]

    -- colors with constraints:
    -- 1) Englishman lives in the red house
    -- 2) Green immediately right of Ivory
    -- 3) Norwegian lives next to the blue house
    colorsFor rs =
      [ cs
      | cs <- permutations positions
      , pos Englishman rs == posC Red cs
      , rightOf (posC Green cs) (posC Ivory cs)
      , nextTo (pos Norwegian rs) (posC Blue cs)
      ]

    -- drinks with constraints:
    -- Coffee in the green house
    -- Ukrainian drinks tea
    -- Milk in the middle house
    drinksFor cs rs =
      [ ds
      | ds <- permutations positions
      , posD Coffee ds == posC Green cs
      , pos Ukrainian rs == posD Tea ds
      , posD Milk ds == middlePos
      ]

    -- smokes with constraints:
    -- Old Gold -> Snails (will be used later with pets)
    -- Kools in yellow house
    -- Lucky Strike -> Orange juice
    -- Japanese -> Parliaments
    smokesFor cs rs ds =
      [ ss
      | ss <- permutations positions
      , posS Kools ss == posC Yellow cs
      , posS LuckyStrike ss == posD OrangeJuice ds
      , pos Japanese rs == posS Parliaments ss
      ]

    -- pets with constraints:
    -- Spaniard owns dog
    petsFor =
      [ ps
      | ps <- permutations positions
      ]

    -- try all consistent combinations with remaining constraints:
    (rs, cs, ds, ss, ps) =
      head
        [ (rs', cs', ds', ss', ps')
        | rs' <- residentsPerms
        , cs' <- colorsFor rs'
        , ds' <- drinksFor cs' rs'
        , ss' <- smokesFor cs' rs' ds'
        , ps' <- petsFor
        , pos Spaniard rs' == posP Dog ps'
        , posS OldGold ss' == posP Snails ps'
        , nextTo (posS Chesterfields ss') (posP Fox ps')
        , nextTo (posS Kools ss') (posP Horse ps')
        ]

    waterPos = posD Water ds
    zebraPos = posP Zebra ps

    -- Lookup helpers
    pos :: Resident -> [Int] -> Int
    pos r rs = indexOf (fromEnum r) rs

    posC :: Color -> [Int] -> Int
    posC c cs = indexOf (fromEnum c) cs

    posP :: Pet -> [Int] -> Int
    posP p ps' = indexOf (fromEnum p) ps'

    posD :: Drink -> [Int] -> Int
    posD d ds' = indexOf (fromEnum d) ds'

    posS :: Smoke -> [Int] -> Int
    posS s ss' = indexOf (fromEnum s) ss'

    indexOf :: Int -> [Int] -> Int
    indexOf i xs = xs !! i

    -- To map position back to resident, invert rs
    toResident :: Int -> Resident
    toResident p = toEnum (fromJust (elemIndex p rs))
