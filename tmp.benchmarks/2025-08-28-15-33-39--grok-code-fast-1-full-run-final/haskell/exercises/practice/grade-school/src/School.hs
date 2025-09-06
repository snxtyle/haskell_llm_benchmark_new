module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype School = School (Map.Map Int (Set.Set String))

add :: Int -> String -> School -> School
add gradeNum student (School m) = School $ Map.insertWith Set.union gradeNum (Set.singleton student) m

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School m) = maybe [] Set.toList (Map.lookup gradeNum m)

sorted :: School -> [(Int, [String])]
sorted (School m) = map (\(g, s) -> (g, Set.toList s)) (Map.toList m)
