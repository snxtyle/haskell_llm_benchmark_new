module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort, lookup)

-- | All possible kinds of plants that can appear in the garden.
data Plant
    = Clover
    | Grass
    | Radishes
    | Violets
    deriving (Eq, Show)

-- | Internal representation of a garden: an association list that maps
-- a student's name to the list of plants that belong to them.
newtype Garden = Garden [(String, [Plant])]

-- | The default list of students, in alphabetical order.
defaultStudents :: [String]
defaultStudents =
    [ "Alice", "Bob", "Charlie", "David", "Eve", "Fred"
    , "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
    ]

-- | Build a garden from a (possibly empty) list of student names and a
-- diagram describing the plants (two lines, characters C G R V).
--
-- If the list of students is empty, the default alphabetical list is used.
-- Otherwise the supplied names are first sorted alphabetically, then
-- assigned two cups per row (four plants in total) from left to right.
garden :: [String]   -- ^ List of students (may be empty for default list)
       -> String     -- ^ Diagram, expected to contain exactly two lines
       -> Garden
garden studentsRaw diagram =
    Garden studentPlants
  where
    students = if null studentsRaw then defaultStudents else sort studentsRaw

    -- Split the diagram into its two rows.
    (row1, row2) = case lines diagram of
        (r1:r2:_) -> (r1, r2)
        _         -> error "Diagram must contain exactly two rows."

    -- Convert a character code into its corresponding Plant.
    charToPlant :: Char -> Plant
    charToPlant 'C' = Clover
    charToPlant 'G' = Grass
    charToPlant 'R' = Radishes
    charToPlant 'V' = Violets
    charToPlant c   = error $ "Unknown plant code: " ++ [c]

    -- Retrieve the four plants for the *n*‑th student.
    plantsFor :: Int -> [Plant]
    plantsFor n =
        map charToPlant
            [ row1 !! (2 * n)
            , row1 !! (2 * n + 1)
            , row2 !! (2 * n)
            , row2 !! (2 * n + 1)
            ]

    -- Association list of students with their plants.
    studentPlants :: [(String, [Plant])]
    studentPlants = zipWith (\i s -> (s, plantsFor i)) [0 ..] students

-- | Look up the list of plants owned by the specified student in the garden.
-- If the student is not present in the garden, an empty list is returned.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) = maybe [] id (lookup student g)
