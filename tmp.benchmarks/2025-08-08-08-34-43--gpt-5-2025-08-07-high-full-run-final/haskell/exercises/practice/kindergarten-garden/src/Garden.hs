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

garden :: [String] -> String -> Garden
garden students plantsDiagram =
    let names = sort students
        (row1, row2) = parseRows plantsDiagram
        pairs1 = pairs row1
        pairs2 = pairs row2
        perStudent = zipWith combine pairs1 pairs2
        entries = zip names perStudent
    in Garden entries
  where
    parseRows s =
      case lines s of
        [r1, r2] -> (r1, r2)
        _        -> error "Invalid garden diagram"
    pairs [] = []
    pairs [_] = error "Invalid row length in garden diagram"
    pairs (a:b:rest) = [a, b] : pairs rest
    toPlant c =
      case c of
        'G' -> Grass
        'C' -> Clover
        'R' -> Radishes
        'V' -> Violets
        _   -> error ("Invalid plant code: " ++ [c])
    combine p1 p2 = map toPlant (p1 ++ p2)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden assoc) =
  case assocLookup student assoc of
    Just ps -> ps
    Nothing -> []

assocLookup :: Eq a => a -> [(a, b)] -> Maybe b
assocLookup _ [] = Nothing
assocLookup k ((k', v):xs)
  | k == k'   = Just v
  | otherwise = assocLookup k xs
