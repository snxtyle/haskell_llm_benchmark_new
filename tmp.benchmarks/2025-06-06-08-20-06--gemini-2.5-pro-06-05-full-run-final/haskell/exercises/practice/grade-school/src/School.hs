module School (School, add, empty, grade, sorted) where

import Data.List (insert, sortBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

-- We use a newtype to create a distinct School type, wrapping an association
-- list (a list of key-value pairs). This prevents clients from manipulating
-- the internal list directly, as the `School` constructor is not exported.
-- The list stores pairs of grade numbers (Int) and lists of student names ([String]).
newtype School = School [(Int, [String])]

-- | Add a student to a grade. If the grade does not exist, it is created.
-- The list of students for each grade is kept in alphabetical order.
add :: Int -> String -> School -> School
add gradeNum student (School roster) = School (go roster)
  where
    -- go recursively searches for the correct grade in the roster.
    go [] = [(gradeNum, [student])] -- Grade not found, so we add a new entry.
    go ((g, students):gs)
      | g == gradeNum = (g, insert student students) : gs -- Found the grade, insert student alphabetically.
      | otherwise     = (g, students) : go gs             -- Not the right grade, continue searching.

-- | An empty school roster.
empty :: School
empty = School []

-- | Get all students in a specific grade, sorted alphabetically.
-- Returns an empty list if the grade does not exist.
grade :: Int -> School -> [String]
grade gradeNum (School roster) = fromMaybe [] (lookup gradeNum roster)

-- | Get all students in all grades. The grades are sorted numerically,
-- and the students within each grade are sorted alphabetically.
sorted :: School -> [(Int, [String])]
sorted (School roster) = sortBy (on compare fst) roster
