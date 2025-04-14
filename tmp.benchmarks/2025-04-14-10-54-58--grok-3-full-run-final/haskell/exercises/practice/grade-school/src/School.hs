module School (School, add, empty, grade, sorted) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

-- School is a type synonym for a Map from Int (grade) to [String] (list of student names)
type School = Map Int [String]

-- Add a student to a specific grade
add :: Int -> String -> School -> School
add gradeNum student school =
    let currentStudents = Map.findWithDefault [] gradeNum school
        updatedStudents = sort (student : currentStudents)
    in Map.insert gradeNum updatedStudents school

-- Create an empty school roster
empty :: School
empty = Map.empty

-- Get a list of students in a specific grade
grade :: Int -> School -> [String]
grade gradeNum school = Map.findWithDefault [] gradeNum school

-- Get a sorted list of all students in all grades
sorted :: School -> [(Int, [String])]
sorted school = Map.toAscList school
