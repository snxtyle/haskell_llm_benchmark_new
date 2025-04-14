module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (insert, sort) -- Import insert for efficient sorted addition

-- School is represented as a Map from grade number (Int) to a sorted list of student names ([String])
newtype School = School (Map Int [String]) deriving (Show, Eq)

-- | Add a student to the roster for a given grade.
-- Keeps the list of students for the grade sorted alphabetically.
add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) = School $ Map.alter addStudent gradeNum schoolMap
  where
    -- Function to be used with Map.alter
    addStudent :: Maybe [String] -> Maybe [String]
    addStudent Nothing          = Just [student] -- If grade doesn't exist, create it with the student
    addStudent (Just students) = Just (insert student students) -- If grade exists, insert student maintaining sort order

-- | Create an empty school roster.
empty :: School
empty = School Map.empty

-- | Get a list of all students enrolled in a specific grade.
-- Returns an empty list if the grade doesn't exist.
-- The list returned is sorted alphabetically.
grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) = Map.findWithDefault [] gradeNum schoolMap

-- | Get a sorted list of all students in all grades.
-- Grades are sorted numerically, and students within each grade are sorted alphabetically.
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) = Map.toAscList schoolMap
