module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

newtype School = School (Map.Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) = 
    School $ Map.insertWith addStudent gradeNum [student] schoolMap
  where
    addStudent new old = sort (new ++ old)

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) = 
    case Map.lookup gradeNum schoolMap of
        Nothing -> []
        Just students -> students

sorted :: School -> [(Int, [String])]
sorted (School schoolMap) = Map.toAscList schoolMap
