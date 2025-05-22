module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

data School = School (Map.Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School gradeMap) = 
    School (Map.insertWith (++) gradeNum [student] gradeMap)

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School gradeMap) = 
    sort $ Map.findWithDefault [] gradeNum gradeMap

sorted :: School -> [(Int, [String])]
sorted (School gradeMap) = 
    [(gradeNum, sort students) | (gradeNum, students) <- Map.toAscList gradeMap]
