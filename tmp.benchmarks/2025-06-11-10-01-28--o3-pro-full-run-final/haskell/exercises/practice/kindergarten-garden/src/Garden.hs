module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)

data Plant
    = Clover
    | Grass
    | Radishes
    | Violets
    deriving (Eq, Show)

-- | A very small map-like structure pairing each student with their plants.
newtype Garden = Garden [(String, [Plant])]
    deriving (Eq, Show)

-- | The canonical list of students used when none is supplied.
defaultStudents :: [String]
defaultStudents =
    [ "Alice", "Bob", "Charlie", "David"
    , "Eve", "Fred", "Ginny", "Harriet"
    , "Ileana", "Joseph", "Kincaid", "Larry"
    ]

-- | Build a garden from an (optionally empty) list of student names and the
-- diagram string.  The diagram must contain two lines of equal even length,
-- each character being one of  'G', 'C', 'R', or 'V'.
--
-- The children are assigned cups in alphabetical order regardless of the
-- order in which their names are provided.
garden :: [String]   -- ^ Students
       -> String     -- ^ Diagram
       -> Garden
garden studentsList diagram = Garden assignments
  where
    students = if null studentsList then defaultStudents else sort studentsList
    (row1, row2) = parseDiagram diagram

    pairs1 = chunk row1
    pairs2 = chunk row2

    -- Two cups per row -> four plants per student
    plantsPerStudent = zipWith (++) pairs1 pairs2

    assignments = zip students plantsPerStudent

-- | Retrieve the four plants for the given student.  If the student isn't in
-- the garden we return an empty list (this is friendlier than throwing an
-- exception and is sufficient for the exercise).
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden m) =
    case lookup student m of
        Just ps -> ps
        Nothing -> []

-- Helpers --------------------------------------------------------------------

-- Split the diagram into its two rows, raising an error if the input is
-- malformed.
parseDiagram :: String -> (String, String)
parseDiagram s =
    case lines s of
        (r1:r2:_) -> (r1, r2)
        _         -> error "Diagram must contain exactly two rows"

-- Convert a row string into chunks of two plants.
chunk :: String -> [[Plant]]
chunk [] = []
chunk (a:b:rest) = [charToPlant a, charToPlant b] : chunk rest
chunk _ = error "Row length must be even"

-- Translate a diagram character into a Plant.
charToPlant :: Char -> Plant
charToPlant 'G' = Grass
charToPlant 'C' = Clover
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Unknown plant code: " ++ [c]
