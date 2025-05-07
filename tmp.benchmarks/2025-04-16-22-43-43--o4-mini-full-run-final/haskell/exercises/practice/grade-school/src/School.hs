module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- We represent the school as a map from grade number to a set of student names
newtype School = School (M.Map Int (S.Set String))
  deriving (Eq, Show)

-- An empty school has no grades
empty :: School
empty = School M.empty

-- Add a student to a grade; uses Set union to avoid duplicates
add :: Int -> String -> School -> School
add gradeNum student (School m) =
  School (M.insertWith S.union gradeNum (S.singleton student) m)

-- Get the list of students in a grade, sorted alphabetically
grade :: Int -> School -> [String]
grade gradeNum (School m) =
  case M.lookup gradeNum m of
    Nothing -> []
    Just names -> S.toAscList names

-- Get all grades with their students, grades in ascending order,
-- and students within each grade sorted alphabetically
sorted :: School -> [(Int, [String])]
sorted (School m) =
  [ (g, S.toAscList s) | (g, s) <- M.toAscList m ]
