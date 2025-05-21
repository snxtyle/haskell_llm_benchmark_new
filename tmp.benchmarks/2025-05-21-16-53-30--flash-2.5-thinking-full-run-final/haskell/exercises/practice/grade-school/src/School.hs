module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Maybe (fromMaybe)

-- School is a newtype wrapper around a Map from grade number (Int) to a list of student names ([String]).
-- The student names within each list will be kept sorted alphabetically.
newtype School = School (Map.Map Int [String])

-- | 'empty' returns an empty school roster.
empty :: School
empty = School Map.empty

-- | 'add' adds a student to a specific grade in the school roster.
-- Students within a grade are kept sorted alphabetically.
add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) =
  School $ Map.alter (Just . sort . (student :) . fromMaybe []) gradeNum schoolMap

-- | 'grade' returns the list of students for a given grade.
-- If the grade does not exist, an empty list is returned.
grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) =
  Map.findWithDefault [] gradeNum schoolMap

-- | 'sorted' returns a list of all grades and their students,
-- sorted by grade number and then by student name within each grade.
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) =
  Map.toList schoolMap
