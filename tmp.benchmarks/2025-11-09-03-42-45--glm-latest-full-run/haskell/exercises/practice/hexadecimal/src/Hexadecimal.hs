module Hexadecimal (hexToInt) where

import Data.Char (toLower, isDigit)
import Data.Foldable (foldl')

-- | Converts a hexadecimal string to its decimal integer equivalent.
--   Handles invalid strings by returning 0.
hexToInt :: String -> Int
hexToInt hexString =
    case mapM charToValue hexString of
        Nothing      -> 0 -- Invalid character found in the string
        Just digits  -> foldl' (\acc d -> acc * 16 + d) 0 digits

-- | Helper function to convert a single hex character to its integer value.
--   Returns Nothing if the character is not a valid hex digit.
charToValue :: Char -> Maybe Int
charToValue c
    | isDigit c = Just (read [c])
    | 'a' <= lc && lc <= 'f' = Just (10 + fromEnum lc - fromEnum 'a')
    | otherwise = Nothing
    where lc = toLower c
