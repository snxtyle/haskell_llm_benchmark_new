module ZebraPuzzle (Resident(..), Solution(..), solve) where

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

-- Additional data types for other properties
data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Eq, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Show)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Eq, Show)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Show)

-- Represents a house with all its properties
data House = House { position :: Int
                   , color :: Color
                   , resident :: Resident
                   , pet :: Pet
                   , drink :: Drink
                   , smoke :: Smoke
                   } deriving (Eq, Show)

-- All possible values for each property
allColors :: [Color]
allColors = [Red, Green, Ivory, Yellow, Blue]

allResidents :: [Resident]
allResidents = [Englishman, Spaniard, Ukrainian, Norwegian, Japanese]

allPets :: [Pet]
allPets = [Dog, Snails, Fox, Horse, Zebra]

allDrinks :: [Drink]
allDrinks = [Coffee, Tea, Milk, OrangeJuice, Water]

allSmokes :: [Smoke]
allSmokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments]

-- Solve the puzzle
solve :: Solution
solve = 
    let houses = findSolution
        waterHouse = head [h | h <- houses, drink h == Water]
        zebraHouse = head [h | h <- houses, pet h == Zebra]
    in Solution { waterDrinker = resident waterHouse
                , zebraOwner = resident zebraHouse }

-- Find solution using backtracking with constraints
findSolution :: [House]
findSolution = head $ backtrack 1 [] allColors allResidents allPets allDrinks allSmokes

-- Backtracking search
backtrack :: Int -> [House] -> [Color] -> [Resident] -> [Pet] -> [Drink] -> [Smoke] -> [[House]]
backtrack 6 houses _ _ _ _ _ = if checkAllConstraints houses then [houses] else []
backtrack pos houses colors residents pets drinks smokes =
    concat [ backtrack (pos + 1) (house:houses) colors' residents' pets' drinks' smokes'
           | c <- colors, let colors' = filter (/= c) colors
           , r <- residents, let residents' = filter (/= r) residents
           , p <- pets, let pets' = filter (/= p) pets
           , d <- drinks, let drinks' = filter (/= d) drinks
           , s <- smokes, let smokes' = filter (/= s) smokes
           , let house = House pos c r p d s
           , checkPartialConstraints (house:houses)
           ]

-- Check constraints that can be applied to partial solutions
checkPartialConstraints :: [House] -> Bool
checkPartialConstraints houses =
    all (\f -> f houses) [
        -- 2. The Englishman lives in the red house.
        \hs -> all (\h -> resident h /= Englishman || color h == Red) hs,
        
        -- 3. The Spaniard owns the dog.
        \hs -> all (\h -> resident h /= Spaniard || pet h == Dog) hs,
        
        -- 4. Coffee is drunk in the green house.
        \hs -> all (\h -> drink h /= Coffee || color h == Green) hs,
        
        -- 5. The Ukrainian drinks tea.
        \hs -> all (\h -> resident h /= Ukrainian || drink h == Tea) hs,
        
        -- 6. The green house is immediately to the right of the ivory house (partial check)
        \hs -> all (\h1 -> color h1 /= Ivory || 
                          all (\h2 -> position h2 /= position h1 + 1 || color h2 /= Green) hs) hs,
        
        -- 7. The Old Gold smoker owns snails.
        \hs -> all (\h -> smoke h /= OldGold || pet h == Snails) hs,
        
        -- 8. Kools are smoked in the yellow house.
        \hs -> all (\h -> smoke h /= Kools || color h == Yellow) hs,
        
        -- 9. Milk is drunk in the middle house.
        \hs -> all (\h -> position h /= 3 || drink h == Milk) hs,
        
        -- 10. The Norwegian lives in the first house.
        \hs -> all (\h -> position h /= 1 || resident h == Norwegian) hs,
        
        -- 13. The Lucky Strike smoker drinks orange juice.
        \hs -> all (\h -> smoke h /= LuckyStrike || drink h == OrangeJuice) hs,
        
        -- 14. The Japanese smokes Parliaments.
        \hs -> all (\h -> resident h /= Japanese || smoke h == Parliaments) hs
    ]

-- Check all constraints including those that require complete solution
checkAllConstraints :: [House] -> Bool
checkAllConstraints houses =
    checkPartialConstraints houses &&
    all (\f -> f houses) [
        -- 6. The green house is immediately to the right of the ivory house (complete check)
        \hs -> any (\h1 -> color h1 == Ivory && 
                          any (\h2 -> color h2 == Green && position h2 == position h1 + 1) hs) hs,
        
        -- 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
        \hs -> any (\h1 -> smoke h1 == Chesterfields && 
                          any (\h2 -> pet h2 == Fox && 
                                     (position h2 == position h1 + 1 || position h2 == position h1 - 1)) hs) hs,
        
        -- 12. Kools are smoked in the house next to the house where the horse is kept.
        \hs -> any (\h1 -> smoke h1 == Kools && 
                          any (\h2 -> pet h2 == Horse && 
                                     (position h2 == position h1 + 1 || position h2 == position h1 - 1)) hs) hs,
        
        -- 15. The Norwegian lives next to the blue house.
        \hs -> any (\h1 -> resident h1 == Norwegian && 
                          any (\h2 -> color h2 == Blue && 
                                     (position h2 == position h1 + 1 || position h2 == position h1 - 1)) hs) hs
    ]
