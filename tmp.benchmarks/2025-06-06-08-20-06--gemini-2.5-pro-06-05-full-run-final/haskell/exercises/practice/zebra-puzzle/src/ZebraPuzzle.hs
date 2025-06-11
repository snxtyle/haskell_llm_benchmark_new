module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (findIndex, permutations)

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

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound..maxBound]

solve :: Solution
solve =
  let (residents, pets, drinks) = head solutions
      waterDrinkerResident = fst . head . filter ((== Water) . snd) $ zip residents drinks
      zebraOwnerResident   = fst . head . filter ((== Zebra) . snd) $ zip residents pets
  in Solution { waterDrinker = waterDrinkerResident, zebraOwner = zebraOwnerResident }
  where
    solutions =
      [ (ns, ps, ds)
      -- Generate permutations and apply constraints as early as possible
      -- to prune the search space.
      | ns <- permutations allNations, head ns == Norwegian -- 10. Norwegian in first house
      , cs <- permutations allColors
      , cs !! 1 == Blue         -- 15. Norwegian next to blue house -> house 2 is Blue
      , isRightOf Green Ivory cs -- 6. Green house right of Ivory house
      , paired cs Red ns Englishman -- 2. Englishman in red house
      , ds <- permutations allDrinks
      , ds !! 2 == Milk         -- 9. Milk in middle house
      , paired cs Green ds Coffee -- 4. Coffee in green house
      , paired ns Ukrainian ds Tea -- 5. Ukrainian drinks tea
      , ss <- permutations allSmokes
      , paired cs Yellow ss Kools -- 8. Kools in yellow house
      , paired ns Japanese ss Parliaments -- 14. Japanese smokes Parliaments
      , paired ss LuckyStrike ds OrangeJuice -- 13. Lucky Strike smoker drinks orange juice
      , ps <- permutations allPets
      , paired ns Spaniard ps Dog -- 3. Spaniard owns dog
      , paired ss OldGold ps Snails -- 7. Old Gold smoker owns snails
      , nextTo ss Chesterfields ps Fox -- 11. Chesterfields smoker next to fox owner
      , nextTo ss Kools ps Horse -- 12. Kools smoker next to horse owner
      ]

    allColors  = allValues :: [Color]
    allNations = allValues :: [Resident]
    allPets    = allValues :: [Pet]
    allDrinks  = allValues :: [Drink]
    allSmokes  = allValues :: [Smoke]

    -- Same-house constraints: two properties belong to the same house
    paired p1 v1 p2 v2 = (v1, v2) `elem` zip p1 p2

    -- Neighbor constraints
    -- isRightOf: `right` is immediately to the right of `left`
    isRightOf right left xs = (left, right) `elem` zip xs (tail xs)

    -- nextTo: two properties are in adjacent houses
    nextTo p1 v1 p2 v2 = case (findIndex (== v1) p1, findIndex (== v2) p2) of
                           (Just i1, Just i2) -> abs (i1 - i2) == 1
                           _ -> False
