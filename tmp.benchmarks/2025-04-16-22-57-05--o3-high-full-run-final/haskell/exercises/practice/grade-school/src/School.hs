module School
  ( School
  , add
  , empty
  , grade
  , sorted
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

-- | A school is a mapping from grade numbers to a set of student names.
--   Using 'Set' ensures names are unique and kept in alphabetical order.
newtype School = School (Map Int (Set String))
  deriving (Eq, Show)

-- | The empty school (no students at all).
empty :: School
empty = School Map.empty

-- | Add a student to a given grade.
--   If the student is already present in the grade we leave the roster unchanged.
add :: Int        -- ^ Grade number
    -> String     -- ^ Student name
    -> School     -- ^ Current school
    -> School     -- ^ Updated school
add gradeNum student (School db) =
  School $
    Map.alter update gradeNum db
  where
    update Nothing   = Just (Set.singleton student)
    update (Just ss) = Just (Set.insert student ss)

-- | Get all students enrolled in a specific grade, alphabetically sorted.
grade :: Int -> School -> [String]
grade gradeNum (School db) =
  maybe [] Set.toAscList (Map.lookup gradeNum db)

-- | Get a complete, sorted roster.
--   Grades are returned in ascending numeric order; within each grade
--   students are listed alphabetically.
sorted :: School -> [(Int, [String])]
sorted (School db) =
  [ (g, Set.toAscList names) | (g, names) <- Map.toAscList db ]
