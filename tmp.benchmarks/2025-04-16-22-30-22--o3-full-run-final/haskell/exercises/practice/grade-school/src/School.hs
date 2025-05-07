module School (School, add, empty, grade, sorted) where

import Data.List (sort)

-- | The 'School' type keeps the roster organised by grade.
--   Internally we use a list of pairs.  Each pair contains a grade
--   number and the (alphabetically‑sorted) list of students enrolled
--   in that grade.  The outer list is always kept in ascending order
--   by grade number.
newtype School = School [(Int, [String])]
  deriving (Eq, Show)

-- | An empty school (no grades, no students).
empty :: School
empty = School []

-- | Add a 'student' to the given 'gradeNum'.
--   The student lists are kept alphabetically sorted and duplicates
--   are ignored.
add :: Int -> String -> School -> School
add gradeNum student (School roster) = School (insertGrade roster)
  where
    insertGrade [] = [(gradeNum, [student])]
    insertGrade ((g, names) : rest)
      | gradeNum < g =
          (gradeNum, [student]) : (g, names) : rest
      | gradeNum == g =
          let names' = if student `elem` names
                          then names
                          else sort (student : names)
          in (g, names') : rest
      | otherwise =
          (g, names) : insertGrade rest

-- | Retrieve the students registered in a particular grade.
--   Returns an empty list if the grade does not exist.
grade :: Int -> School -> [String]
grade gradeNum (School roster) =
  case lookup gradeNum roster of
    Just names -> names
    Nothing    -> []

-- | A fully sorted roster:
--   - grades appear in ascending numeric order
--   - students within each grade appear in alphabetical order
sorted :: School -> [(Int, [String])]
sorted (School roster) = roster
