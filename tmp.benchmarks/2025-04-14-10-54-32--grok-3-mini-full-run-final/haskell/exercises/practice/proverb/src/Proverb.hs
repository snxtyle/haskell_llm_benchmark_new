module Proverb(recite) where

recite :: [String] -> String
recite [] = ""  -- Return empty string for empty input
recite xs
  | length xs == 1 = lastLine xs  -- For one element, return just the string without newline
  | otherwise = unlines $  -- For more elements, use unlines to join with newlines
      zipWith makeLine xs (tail xs) ++  -- Generate lines for consecutive pairs
      [lastLine xs]  -- Add the final line

  where
    makeLine a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    lastLine ys = "And all for the want of a " ++ head ys  -- Use head for the first element
