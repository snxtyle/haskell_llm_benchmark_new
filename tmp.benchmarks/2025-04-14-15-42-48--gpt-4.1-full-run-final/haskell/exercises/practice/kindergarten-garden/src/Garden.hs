module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden { studentPlants :: Map.Map String [Plant] }

-- The canonical list of students, in alphabetical order
studentsCanonical :: [String]
studentsCanonical =
    [ "Alice", "Bob", "Charlie", "David", "Eve", "Fred"
    , "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
    ]

-- Map a character to a Plant
charToPlant :: Char -> Maybe Plant
charToPlant 'C' = Just Clover
charToPlant 'G' = Just Grass
charToPlant 'R' = Just Radishes
charToPlant 'V' = Just Violets
charToPlant _   = Nothing

-- Parse the diagram into two rows
parseRows :: String -> [String]
parseRows = lines

-- Given a list of students and a diagram, build the Garden
garden :: [String] -> String -> Garden
garden students diagram =
    let studentList = sort students
        rows = parseRows diagram
        -- Each row must have 2 cups per student
        cupsPerStudent = 2
        -- For each row, split into chunks of 2
        rowChunks = map (chunksOf cupsPerStudent) rows
        -- Transpose so that for each student, we get their cups from both rows
        -- rowChunks :: [[String]] (one list per row, each split into 2-cup chunks)
        -- We want: [[cup1row1, cup1row2], [cup2row1, cup2row2], ...]
        studentCups = zipWith (++) (rowChunks !! 0) (rowChunks !! 1)
        -- Map each student to their 4 cups
        studentToCups = zip studentList studentCups
        -- Map each student to their 4 plants
        studentToPlants = map (\(s, cups) -> (s, map (fromMaybe Grass . charToPlant) cups)) studentToCups
    in Garden (Map.fromList studentToPlants)

-- Helper: split a list into chunks of n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- Lookup a student's plants in the garden
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden m) = Map.findWithDefault [] student m
