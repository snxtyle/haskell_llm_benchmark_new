module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

-- School is a map from grade number to list of student names
-- Students within each grade are kept sorted alphabetically
newtype School = School (Map.Map Int [String])
  deriving (Eq, Show)

-- Add a student to a specific grade, maintaining alphabetical order
add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) = 
  School $ Map.insertWith addSorted gradeNum [student] schoolMap
  where
    -- When combining lists, sort the result to maintain alphabetical order
    addSorted new old = sort (old ++ new)

-- Create an empty school
empty :: School
empty = School Map.empty

-- Get all students in a specific grade
grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) = 
  Map.findWithDefault [] gradeNum schoolMap

-- Get a sorted list of all grades and their students
-- Grades are sorted numerically, students within each grade are already sorted
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) = Map.toAscList schoolMap
