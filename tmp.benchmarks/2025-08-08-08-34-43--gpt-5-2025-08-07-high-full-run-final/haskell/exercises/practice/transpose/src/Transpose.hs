module Transpose (transpose) where

import qualified Data.List as List
import Data.Maybe (isNothing)

transpose :: [String] -> [String]
transpose rows =
  map (map maybeToChar . dropTrailingNothings) (List.transpose padded)
  where
    maxLen :: Int
    maxLen = maximum (0 : map length rows)

    padded :: [[Maybe Char]]
    padded = map padRight rows

    padRight :: String -> [Maybe Char]
    padRight s = map Just s ++ replicate (maxLen - length s) Nothing

    maybeToChar :: Maybe Char -> Char
    maybeToChar (Just c) = c
    maybeToChar Nothing  = ' '

    dropTrailingNothings :: [Maybe a] -> [Maybe a]
    dropTrailingNothings = reverse . dropWhile isNothing . reverse
