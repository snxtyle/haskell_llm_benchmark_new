module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite xs = joinLines (pairLines xs ++ [finalLine xs])
  where
    pairLines :: [String] -> [String]
    pairLines ys =
      [ "For want of a " ++ a ++ " the " ++ b ++ " was lost."
      | (a, b) <- zip ys (drop 1 ys)
      ]

    finalLine :: [String] -> String
    finalLine (y:_) = "And all for the want of a " ++ y ++ "."
    finalLine [] = ""  -- unreachable due to recite [] case

    joinLines :: [String] -> String
    joinLines [] = ""
    joinLines (l:ls) = foldl (\acc s -> acc ++ "\n" ++ s) l ls
