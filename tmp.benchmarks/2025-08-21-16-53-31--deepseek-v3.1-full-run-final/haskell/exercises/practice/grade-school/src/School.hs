module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

-- School is a map from grade numbers to sorted lists of student names
newtype School = School (Map.Map Int [String])

-- Add a student to a grade
add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) = 
    School $ Map.insertWith mergeStudents gradeNum [student] schoolMap
  where
    mergeStudents newStudents oldStudents = sort (newStudents ++ oldStudents)

-- Create an empty school
empty :: School
empty = School Map.empty

-- Get all students in a specific grade
grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) = 
    case Map.lookup gradeNum schoolMap of
        Just students -> students
        Nothing -> []

-- Get all grades with their students, sorted by grade and students alphabetically
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) = Map.toAscList schoolMap
