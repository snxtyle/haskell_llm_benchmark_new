module ZebraPuzzle (Resident(..), Solution(..), solve) where

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = Solution dw zo
  where
    -- Generate all possible arrangements for residents
    residents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]
    residentPerms = [[r1,r2,r3,r4,r5] | r1 <- residents, r2 <- residents, r3 <- residents, r4 <- residents, r5 <- residents,
                                       r1 /= r2, r1 /= r3, r1 /= r4, r1 /= r5,
                                       r2 /= r3, r2 /= r4, r2 /= r5,
                                       r3 /= r4, r3 /= r5,
                                       r4 /= r5]
    
    -- Generate all possible arrangements for each attribute
    colors = permutations ["red", "green", "ivory", "yellow", "blue"]
    pets = permutations ["dog", "snails", "fox", "horse", "zebra"]
    drinks = permutations ["coffee", "tea", "milk", "orangeJuice", "water"]
    smokes = permutations ["oldGold", "kools", "chesterfields", "luckyStrike", "parliaments"]
    
    -- Check all combinations
    validCombination (cs, ps, ds, ss, rs) = 
      -- 2. The Englishman lives in the red house
      (findIndexByResident (== Englishman) rs == findIndexByColor (== "red") cs) &&
      -- 3. The Spaniard owns the dog
      (findIndexByResident (== Spaniard) rs == findIndexByPet (== "dog") ps) &&
      -- 4. Coffee is drunk in the green house
      (findIndexByDrink (== "coffee") ds == findIndexByColor (== "green") cs) &&
      -- 5. The Ukrainian drinks tea
      (findIndexByResident (== Ukrainian) rs == findIndexByDrink (== "tea") ds) &&
      -- 6. The green house is immediately to the right of the ivory house
      (case findIndexByColor (== "ivory") cs of
         Just i -> case findIndexByColor (== "green") cs of
                     Just j -> j == i + 1
                     Nothing -> False
         Nothing -> False) &&
      -- 7. The Old Gold smoker owns snails
      (findIndexBySmoke (== "oldGold") ss == findIndexByPet (== "snails") ps) &&
      -- 8. Kools are smoked in the yellow house
      (findIndexBySmoke (== "kools") ss == findIndexByColor (== "yellow") cs) &&
      -- 9. Milk is drunk in the middle house (index 2)
      (ds !! 2 == "milk") &&
      -- 10. The Norwegian lives in the first house (index 0)
      (rs !! 0 == Norwegian) &&
      -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox
      (adjacent (findIndexBySmoke (== "chesterfields") ss) (findIndexByPet (== "fox") ps)) &&
      -- 12. Kools are smoked in the house next to the house where the horse is kept
      (adjacent (findIndexBySmoke (== "kools") ss) (findIndexByPet (== "horse") ps)) &&
      -- 13. The Lucky Strike smoker drinks orange juice
      (findIndexBySmoke (== "luckyStrike") ss == findIndexByDrink (== "orangeJuice") ds) &&
      -- 14. The Japanese smokes Parliaments
      (findIndexByResident (== Japanese) rs == findIndexBySmoke (== "parliaments") ss) &&
      -- 15. The Norwegian lives next to the blue house
      (adjacent (Just 0) (findIndexByColor (== "blue") cs))
    
    -- Find valid solution
    validSolutions = [(cs, ps, ds, ss, rs) | 
                      cs <- colors, ps <- pets, ds <- drinks, ss <- smokes, rs <- residentPerms,
                      validCombination (cs, ps, ds, ss, rs)]
    
    -- Extract solution
    (solutionColors, solutionPets, solutionDrinks, solutionSmokes, solutionResidents) = head validSolutions
    
    -- Find water drinker and zebra owner
    dw = solutionResidents !! (fromJust $ findIndexByDrink (== "water") solutionDrinks)
    zo = solutionResidents !! (fromJust $ findIndexByPet (== "zebra") solutionPets)
    
    -- Helper functions
    findIndexByColor pred = findIndex pred
    findIndexByPet pred = findIndex pred
    findIndexByDrink pred = findIndex pred
    findIndexBySmoke pred = findIndex pred
    findIndexByResident pred = findIndex pred
    
    findIndex :: (a -> Bool) -> [a] -> Maybe Int
    findIndex pred xs = findIndex' pred xs 0
      where
        findIndex' _ [] _ = Nothing
        findIndex' p (y:ys) i
          | p y = Just i
          | otherwise = findIndex' p ys (i+1)
    
    adjacent :: Maybe Int -> Maybe Int -> Bool
    adjacent (Just i) (Just j) = abs (i - j) == 1
    adjacent _ _ = False
    
    fromJust :: Maybe a -> a
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing"
    
    permutations :: [a] -> [[a]]
    permutations [] = [[]]
    permutations xs = [y:ys | y <- xs, ys <- permutations (remove y xs)]
      where
        remove _ [] = []
        remove y (x:xs)
          | x == y = xs
          | otherwise = x : remove y xs
