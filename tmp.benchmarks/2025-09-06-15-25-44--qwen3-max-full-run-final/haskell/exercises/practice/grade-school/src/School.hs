module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

-- School is a map from grade numbers to sorted lists of student names
type School = Map.Map Int [String]

-- Add a student to a grade, maintaining alphabetical order within the grade
add :: Int -> String -> School -> School
add gradeNum student school =
    let currentStudents = Map.findWithDefault [] gradeNum school
        updatedStudents = sort (student : currentStudents)
    in Map.insert gradeNum updatedStudents school

-- Create an empty school
empty :: School
empty = Map.empty

-- Get all students in a specific grade (returns empty list if grade doesn't exist)
grade :: Int -> School -> [String]
grade gradeNum school = Map.findWithDefault [] gradeNum school

-- Get a sorted list of all students in all grades
-- Grades are sorted numerically, students within grades are sorted alphabetically
sorted :: School -> [(Int, [String])]
sorted school = Map.toAscList school
