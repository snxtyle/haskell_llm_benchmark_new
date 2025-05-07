module School (School, add, empty, grade, sorted) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- School is represented as a Map from grade number (Int) to a list of student names ([String]).
-- The list of student names for each grade is kept sorted alphabetically.
newtype School = School (Map Int [String]) deriving (Show, Eq)

-- | Creates an empty school roster.
empty :: School
empty = School Map.empty

-- | Adds a student to a specific grade in the school roster.
-- If the grade already exists, the student is added to the list of students for that grade.
-- The list of students for each grade is maintained in alphabetical order.
add :: Int         -- ^ Grade number
    -> String      -- ^ Student's name
    -> School      -- ^ The current school roster
    -> School      -- ^ The new school roster with the student added
add gradeNum student (School schoolMap) =
    School (Map.insertWith (\newStudents oldStudents -> sort (head newStudents : oldStudents)) gradeNum [student] schoolMap)
    -- Explanation of Map.insertWith:
    --   - gradeNum: The key to insert/update.
    --   - [student]: The value to insert if the key is new. This also becomes the 'newStudents' argument
    --                for the combining function if the key already exists.
    --   - (\newStudents oldStudents -> sort (head newStudents : oldStudents)): The combining function.
    --     - If gradeNum is new, [student] is inserted.
    --     - If gradeNum exists, oldStudents is the existing list of students.
    --       newStudents is [student]. So, (head newStudents) is 'student'.
    --       The new list becomes sort (student : oldStudents).
    --   - schoolMap: The map to operate on.

-- | Retrieves a list of all students enrolled in a specific grade.
-- The list is sorted alphabetically by student name.
-- Returns an empty list if the grade does not exist or has no students.
grade :: Int         -- ^ Grade number
      -> School      -- ^ The school roster
      -> [String]    -- ^ List of students in the specified grade, sorted alphabetically
grade gradeNum (School schoolMap) =
    Map.findWithDefault [] gradeNum schoolMap
    -- Map.findWithDefault returns the value associated with gradeNum,
    -- or an empty list [] if gradeNum is not in the map.
    -- The list of students is already sorted due to the 'add' function.

-- | Retrieves a sorted list of all students in all grades.
-- Grades are sorted numerically (1, 2, 3, etc.).
-- Students within each grade are sorted alphabetically by name.
sorted :: School -> [(Int, [String])]
sorted (School schoolMap) =
    Map.toAscList schoolMap
    -- Map.toAscList converts the map into a list of (key, value) pairs,
    -- sorted by key (grade number).
    -- The student lists (values) are already sorted alphabetically by the 'add' function.
