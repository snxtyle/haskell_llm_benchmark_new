module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)

-- A School is a map from grade numbers to a list of student names in that grade.
newtype School = School (M.Map Int [String]) deriving (Show)

-- Add a student to a specific grade in the school.
add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) =
    School $ M.insertWith (++) gradeNum [student] schoolMap

-- Create an empty school.
empty :: School
empty = School M.empty

-- Get a list of all students in a specific grade.
-- Returns an empty list if the grade has no students.
grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) = M.findWithDefault [] gradeNum schoolMap

-- Get a sorted list of all students in all grades.
-- The grades are sorted numerically, and the students within each grade are sorted alphabetically.
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) =
    let gradesAndStudents = M.toList schoolMap
    in map (\(g, students) -> (g, sort students)) gradesAndStudents
