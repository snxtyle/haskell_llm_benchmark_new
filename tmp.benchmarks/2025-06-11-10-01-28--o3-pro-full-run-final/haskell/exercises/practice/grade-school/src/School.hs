module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.List       (sort)

-- | A School stores a mapping from grade numbers to the (alphabetically
--   sorted, duplicate-free) list of students in that grade.
newtype School = School (Map Int [String])
  deriving (Eq, Show)

-- | An empty school has no students.
empty :: School
empty = School Map.empty

-- | Add a student to a grade.
--   If the student is already present in that grade the roster is unchanged.
add :: Int -> String -> School -> School
add gradeNum student (School roster) = School newRoster
  where
    currentStudents = Map.findWithDefault [] gradeNum roster
    alreadyPresent  = student `elem` currentStudents
    updatedStudents = if alreadyPresent
                        then currentStudents
                        else sort (student : currentStudents)
    newRoster       = Map.insert gradeNum updatedStudents roster

-- | Retrieve the list of students in a grade (alphabetically sorted).
grade :: Int -> School -> [String]
grade gradeNum (School roster) = Map.findWithDefault [] gradeNum roster

-- | Return the entire roster as a list of (grade, students) pairs,
--   with grades in ascending order and each student list alphabetically sorted.
sorted :: School -> [(Int, [String])]
sorted (School roster) = Map.toAscList roster
