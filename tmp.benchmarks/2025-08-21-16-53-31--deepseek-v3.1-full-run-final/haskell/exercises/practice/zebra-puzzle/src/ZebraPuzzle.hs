module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List (delete, permutations)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Define other attribute types
data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show, Enum, Bounded)
data Pet = Dog | Snails | Fox | Horse | Zebra deriving (Eq, Show, Enum, Bounded)
data Drink = Coffee | Tea | Milk | OrangeJuice | Water deriving (Eq, Show, Enum, Bounded)
data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments deriving (Eq, Show, Enum, Bounded)

-- A house has all attributes
data House = House { nationality :: Resident
                   , color :: Color
                   , pet :: Pet
                   , drink :: Drink
                   , smoke :: Smoke
                   } deriving (Eq, Show)

solve :: Solution
solve = 
  let -- Generate all possible houses with constraints applied early
      solutions = do
        -- House order is fixed: 1, 2, 3, 4, 5
        -- 10. The Norwegian lives in the first house.
        let house1Nat = Norwegian
        
        -- 9. Milk is drunk in the middle house (house 3)
        let house3Drink = Milk
        
        -- 15. The Norwegian lives next to the blue house.
        -- Since Norwegian is in house 1, blue house must be house 2
        let house2Color = Blue
        
        -- Generate remaining colors with constraints
        colors2 <- delete house2Color [Red, Green, Ivory, Yellow, Blue]
        colors3 <- delete colors2 [Red, Green, Ivory, Yellow, Blue]
        colors4 <- delete colors3 [Red, Green, Ivory, Yellow, Blue]
        colors5 <- delete colors4 [Red, Green, Ivory, Yellow, Blue]
        let colors = [colors2, colors3, colors4, colors5] -- house1 color will be determined
        
        -- 6. The green house is immediately to the right of the ivory house.
        -- Find positions where green is right of ivory
        let colorPerms = filter (\(ivoryPos, greenPos) -> greenPos == ivoryPos + 1) $
                         [(i, j) | i <- [0..3], j <- [0..3], i /= j]
        
        -- For each valid ivory/green position, assign colors
        (ivoryPos, greenPos) <- colorPerms
        let tempColors = take 4 colors
        guard (tempColors !! ivoryPos == Ivory && tempColors !! greenPos == Green)
        
        -- 2. The Englishman lives in the red house.
        -- Assign nationalities
        nats <- permutations [Englishman, Spaniard, Ukrainian, Japanese] -- Norwegian is already in house1
        let nationalities = Norwegian : nats
        
        -- 4. Coffee is drunk in the green house.
        -- 5. The Ukrainian drinks tea.
        -- Assign drinks
        drinks <- permutations [Coffee, Tea, OrangeJuice, Water] -- Milk is already in house3
        let allDrinks = take 2 drinks ++ [Milk] ++ drop 2 drinks
        
        -- 13. The Lucky Strike smoker drinks orange juice.
        -- Assign smokes
        smokes <- permutations [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]
        
        -- 14. The Japanese smokes Parliaments.
        guard (any (\(nat, smokeVal) -> nat == Japanese && smokeVal == Parliaments) $ zip nationalities smokes)
        
        -- 8. Kools are smoked in the yellow house.
        guard (any (\(col, smokeVal) -> col == Yellow && smokeVal == Kools) $ zip (house2Color : colors) smokes)
        
        -- 7. The Old Gold smoker owns snails.
        -- Assign pets
        pets <- permutations [Dog, Snails, Fox, Horse, Zebra]
        
        -- 3. The Spaniard owns the dog.
        guard (any (\(nat, petVal) -> nat == Spaniard && petVal == Dog) $ zip nationalities pets)
        
        -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
        let chesterIndex = head [i | (i, s) <- zip [0..] smokes, s == Chesterfields]
        let foxIndex = head [i | (i, p) <- zip [0..] pets, p == Fox]
        guard (abs (chesterIndex - foxIndex) == 1)
        
        -- 12. Kools are smoked in the house next to the house where the horse is kept.
        let koolsIndex = head [i | (i, s) <- zip [0..] smokes, s == Kools]
        let horseIndex = head [i | (i, p) <- zip [0..] pets, p == Horse]
        guard (abs (koolsIndex - horseIndex) == 1)
        
        -- Create the houses
        let houses = zipWith5 House nationalities (house2Color : colors) pets allDrinks smokes
        
        -- Return the solution
        return houses
      
      -- Get the first valid solution
      solutionHouses = head solutions
      
      -- Find who drinks water
      waterDrinkerResident = nationality . head $ filter (\h -> drink h == Water) solutionHouses
      
      -- Find who owns the zebra
      zebraOwnerResident = nationality . head $ filter (\h -> pet h == Zebra) solutionHouses
      
  in Solution waterDrinkerResident zebraOwnerResident
