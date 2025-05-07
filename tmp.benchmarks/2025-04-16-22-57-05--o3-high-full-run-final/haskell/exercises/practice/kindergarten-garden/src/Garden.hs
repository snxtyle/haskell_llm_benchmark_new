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

newtype Garden = Garden [(String, [Plant])]

-- | The default list of students used when none is supplied.
defaultStudents :: [String]
defaultStudents =
    [ "Alice"
    , "Bob"
    , "Charlie"
    , "David"
    , "Eve"
    , "Fred"
    , "Ginny"
    , "Harriet"
    , "Ileana"
    , "Joseph"
    , "Kincaid"
    , "Larry"
    ]

-- | Construct a garden from a (possibly empty) list of students and a diagram.
--   Students are sorted alphabetically before the cups are assigned.
--   If the supplied list is empty, the default student list is used.
garden :: [String] -> String -> Garden
garden students diagram =
    let names = sort (if null students then defaultStudents else students)
        rows  = filter (not . null) (lines diagram)   -- remove possible empty lines
     in case rows of
            [row1, row2] ->
                let assignments = zipWith (\name idx -> (name, plantsFor idx row1 row2)) names [0..]
                 in Garden assignments
            _ -> error "Diagram must contain exactly two non‑empty rows."
  where
    -- Convert the four characters belonging to the n-th student into Plants.
    plantsFor :: Int -> String -> String -> [Plant]
    plantsFor n r1 r2 =
        let i = 2 * n
         in map charToPlant [r1 !! i, r1 !! (i + 1), r2 !! i, r2 !! (i + 1)]

    -- Translate a diagram character to its corresponding Plant.
    charToPlant :: Char -> Plant
    charToPlant c =
        case c of
            'G' -> Grass
            'C' -> Clover
            'R' -> Radishes
            'V' -> Violets
            _   -> error $ "Invalid plant code: " ++ [c]

-- | Retrieve the list of plants for a given student.
--   Returns an empty list if the student is not found in the garden.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) =
    case lookup student g of
        Just ps -> ps
        Nothing -> []
