module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Keep Garden abstract; internally it's a map from student to their plants
newtype Garden = Garden (Map.Map String [Plant])

defaultStudents :: [String]
defaultStudents =
    [ "Alice", "Bob", "Charlie", "David", "Eve", "Fred"
    , "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
    ]

-- Public API: build a Garden from a list of students and a diagram string
garden :: [String] -> String -> Garden
garden students plantsDiagram =
    let students' = if null students then defaultStudents else List.sort students
        (row1, row2) = parseRows plantsDiagram
        pairs1 = twoByTwo row1
        pairs2 = twoByTwo row2
        -- For each student index i, collect the two cups from row1 and row2
        studentPlants = zipWith combine students' (zip pairs1 pairs2)
        m = Map.fromList studentPlants
    in Garden m
  where
    combine :: String -> ((Plant, Plant), (Plant, Plant)) -> (String, [Plant])
    combine name ((a,b),(c,d)) = (name, [a,b,c,d])

-- Lookup a student's plants in the Garden
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden m) = Map.findWithDefault [] student m

-- Helpers

parseRows :: String -> ([Plant], [Plant])
parseRows s =
    case lines s of
      [r1, r2] -> (map toPlant r1, map toPlant r2)
      _        -> ([], [])
  where
    toPlant :: Char -> Plant
    toPlant 'G' = Grass
    toPlant 'C' = Clover
    toPlant 'R' = Radishes
    toPlant 'V' = Violets
    toPlant _   = Grass -- default fallback, though inputs are expected valid

twoByTwo :: [a] -> [(a, a)]
twoByTwo (x:y:rest) = (x,y) : twoByTwo rest
twoByTwo _          = []
