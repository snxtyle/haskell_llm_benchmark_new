module Transpose (transpose) where

import Data.Maybe (fromMaybe, isNothing)

transpose :: [String] -> [String]
transpose xs =
  let maxLen = maximum (0 : map length xs)
      buildRow i =
        let column = [ if i < length s then Just (s !! i) else Nothing | s <- xs ]
            trimmed = trimEndNothings column
        in map (fromMaybe ' ') trimmed
  in [ buildRow i | i <- [0 .. maxLen - 1] ]

trimEndNothings :: [Maybe a] -> [Maybe a]
trimEndNothings = reverse . dropWhile isNothing . reverse
