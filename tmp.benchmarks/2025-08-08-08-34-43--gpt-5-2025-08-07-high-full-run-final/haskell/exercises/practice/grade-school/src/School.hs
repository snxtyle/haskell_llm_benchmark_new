module School (School, add, empty, grade, sorted) where

import Data.List (sort, sortOn)

-- Represent each student as a (grade, name) pair.
-- The constructor is not exported, so the internal structure can't be mutated externally.
newtype School = School [(Int, String)]
  deriving (Eq, Show)

empty :: School
empty = School []

-- Add a student to a grade, ensuring the student is in only one grade.
add :: Int -> String -> School -> School
add gradeNum student (School pairs) =
  let pairsWithoutStudent = filter ((/= student) . snd) pairs
  in School ((gradeNum, student) : pairsWithoutStudent)

-- Get alphabetically sorted list of students in a grade.
grade :: Int -> School -> [String]
grade gradeNum (School pairs) =
  sort [s | (g, s) <- pairs, g == gradeNum]

-- Get grades sorted ascending with alphabetically sorted students per grade.
sorted :: School -> [(Int, [String])]
sorted (School pairs) =
  let insert (g, s) [] = [(g, [s])]
      insert (g, s) ((g', ss) : rest)
        | g == g'   = (g', s : ss) : rest
        | otherwise = (g', ss) : insert (g, s) rest

      buckets = foldr insert [] pairs
      finalize = map (\(g, ss) -> (g, sort ss)) buckets
  in sortOn fst finalize
