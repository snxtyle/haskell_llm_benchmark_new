module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- We represent the garden as an association list from student names to their
-- list of plants, wrapped in a newtype for type safety.
newtype Garden = Garden [(String, [Plant])] deriving (Show)

-- Helper to convert a character to a Plant.
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Invalid plant character: " ++ [c]

garden :: [String] -> String -> Garden
garden students plantsDiagram = Garden (zip (sort students) allPlants)
  where
    -- The diagram is two lines, one for each row of cups.
    [row1, row2] = lines plantsDiagram

    -- Helper to split a list into chunks of a given size.
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

    -- Each student gets two cups from each row.
    -- We chunk each row into pairs of characters.
    row1Cups = chunksOf 2 row1
    row2Cups = chunksOf 2 row2

    -- We combine the cups for each student. `zipWith` pairs the first chunk
    -- of row1 with the first of row2, and so on.
    -- e.g., ["VR", "CG"], ["VC", "CR"] becomes ["VRVC", "CGCR"]
    studentCupChars = zipWith (++) row1Cups row2Cups

    -- Finally, we convert the character strings for each student into
    -- lists of `Plant` data types.
    allPlants = map (map charToPlant) studentCupChars

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden gardenData) =
    case lookup student gardenData of
        Just plants -> plants
        Nothing     -> error $ "Student not found: " ++ student
