module School (School, add, empty, grade, sorted) where

import qualified Data.Map.Strict as Map
import Data.List (sort)

newtype School = School (Map.Map Int [String])

empty :: School
empty = School Map.empty

add :: Int -> String -> School -> School
add gradeNum student (School school) =
  let updatedStudents = sort (student : Map.findWithDefault [] gradeNum school)
      updatedSchool = Map.insert gradeNum updatedStudents school
  in School updatedSchool

grade :: Int -> School -> [String]
grade gradeNum (School school) = Map.findWithDefault [] gradeNum school

sorted :: School -> [(Int, [String])]
sorted (School school) = Map.toAscList school
