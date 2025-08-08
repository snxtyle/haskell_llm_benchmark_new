module School (School, add, empty, grade, sorted) where

import Data.List (insert)

newtype School = School [(Int, [String])]
  deriving (Eq, Show)

empty :: School
empty = School []

add :: Int -> String -> School -> School
add g s (School xs) = School (go xs)
  where
    go [] = [(g, [s])]
    go ((g', ss):rest)
      | g == g'   = (g, insertStudent s ss) : rest
      | g < g'    = (g, [s]) : (g', ss) : rest
      | otherwise = (g', ss) : go rest

    insertStudent name names
      | name `elem` names = names
      | otherwise         = insert name names

grade :: Int -> School -> [String]
grade g (School xs) =
  case lookup g xs of
    Just names -> names
    Nothing    -> []

sorted :: School -> [(Int, [String])]
sorted (School xs) = xs
