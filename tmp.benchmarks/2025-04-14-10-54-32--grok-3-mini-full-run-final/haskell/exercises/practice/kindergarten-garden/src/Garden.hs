module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort, unfoldr)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [(String, [Plant])]

garden :: [String] -> String -> Garden
garden students plantsStr
    | length plantsStr /= 48 = error "Diagram must be exactly 48 characters."
    | otherwise =
        let row1 = take 24 plantsStr
            row2 = drop 24 plantsStr
            sortedStudents = sort students  -- Sort students alphabetically
            plantsPerStudent = 2  -- 2 plants per row per student
            row1Pairs = chunksOf plantsPerStudent row1  -- Split row1 into pairs
            row2Pairs = chunksOf plantsPerStudent row2  -- Split row2 into pairs
            studentPlants = zip sortedStudents (zipWith (++) (map (map charToPlant) row1Pairs) (map (map charToPlant) row2Pairs))
        in Garden studentPlants
  where
    chunksOf n xs = take (length xs `div` n) $ unfoldr (Just . splitAt n) xs  -- Helper to split list into chunks
    charToPlant 'G' = Grass
    charToPlant 'C' = Clover
    charToPlant 'R' = Radishes
    charToPlant 'V' = Violets
    charToPlant _   = error "Invalid plant code."

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden plantsList) =
    case lookup student plantsList of
        Just plants -> plants
        Nothing -> error "Student not found."
