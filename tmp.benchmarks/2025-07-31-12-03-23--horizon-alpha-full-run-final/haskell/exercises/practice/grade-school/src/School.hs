module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as M
import Data.List (insert, sort)

-- Represent the school roster as a map from grade number to list of student names.
-- We will maintain the invariant that each list of students is sorted alphabetically
-- and contains no duplicates.
newtype School = School (M.Map Int [String])
  deriving (Eq, Show)

-- Create an empty school.
empty :: School
empty = School M.empty

-- Add a student to a grade. If the student is already enrolled in that grade,
-- do not add a duplicate. Maintain alphabetical ordering within the grade.
add :: Int -> String -> School -> School
add gradeNum student (School m) =
  let updated = M.alter insertStudent gradeNum m
  in School updated
  where
    insertStudent :: Maybe [String] -> Maybe [String]
    insertStudent Nothing = Just [student]
    insertStudent (Just xs)
      | student `elem` xs = Just xs
      | otherwise = Just (insert student xs)

-- Get a list of students in the specified grade.
-- Returns an empty list if the grade does not exist.
grade :: Int -> School -> [String]
grade gradeNum (School m) = M.findWithDefault [] gradeNum m

-- Get a sorted list of all students in all grades.
-- Grades are sorted numerically, and students within each grade are sorted alphabetically.
sorted :: School -> [(Int, [String])]
sorted (School m) =
  -- Ensure each list is sorted (in case external code constructed School manually)
  let normalized = fmap sort m
  in M.toAscList normalized
