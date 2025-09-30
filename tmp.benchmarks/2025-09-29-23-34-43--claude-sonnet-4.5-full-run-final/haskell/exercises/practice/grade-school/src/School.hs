module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

data School = School (Map.Map Int [String])
  deriving (Show, Eq)

add :: Int -> String -> School -> School
add gradeNum student (School schoolMap) =
  School (Map.insertWith (++) gradeNum [student] schoolMap)

empty :: School
empty = School Map.empty

grade :: Int -> School -> [String]
grade gradeNum (School schoolMap) =
  sort $ Map.findWithDefault [] gradeNum schoolMap

sorted :: School -> [(Int, [String])]
sorted (School schoolMap) =
  Map.toAscList $ Map.map sort schoolMap
