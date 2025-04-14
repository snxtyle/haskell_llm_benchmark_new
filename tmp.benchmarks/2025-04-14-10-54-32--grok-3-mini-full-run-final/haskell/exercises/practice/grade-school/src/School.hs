module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)

data School = School (Map.Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School map) =
    let currentList = Map.findWithDefault [] gradeNum map  -- Get the current list or empty
        newList = sort (student : currentList)  -- Add student and sort the list
    in School (Map.insert gradeNum newList map)  -- Insert the updated list

empty :: School
empty = School Map.empty  -- Create an empty school

grade :: Int -> School -> [String]
grade gradeNum (School map) =
    case Map.lookup gradeNum map of
        Just students -> students  -- Return the list (already sorted from add)
        Nothing -> []  -- Return empty list if grade doesn't exist

sorted :: School -> [(Int, [String])]
sorted (School map) = Map.toList map  -- Map.toList returns a list sorted by keys (grades)
-- Each value (list of students) is already sorted due to the add function
