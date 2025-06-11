module Proverb (recite) where

import Data.List (intercalate)

-- | Generate the proverb corresponding to the supplied list of words.
--   An empty list yields an empty string.
--   A singleton list yields only the concluding line.
recite :: [String] -> String
recite []      = ""
recite [x]     = concludingLine x
recite xs      = intercalate "\n" $ pairLines xs ++ [concludingLine (head xs)]

-- Create a line for each successive pair of items in the list.
pairLines :: [String] -> [String]
pairLines ys = zipWith line ys (tail ys)

line :: String -> String -> String
line a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."

concludingLine :: String -> String
concludingLine first = "And all for the want of a " ++ first ++ "."
