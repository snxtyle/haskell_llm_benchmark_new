module Hexadecimal (hexToInt) where

import Data.Char (digitToInt, isHexDigit)
import Data.Maybe (fromMaybe)

-- | Converts a hexadecimal string representation to its decimal integer value.
-- Returns 0 if the input string contains any invalid hexadecimal characters.
hexToInt :: String -> Int
hexToInt hexString = fromMaybe 0 (foldl processChar (Just 0) hexString)
  where
    -- | Processes a single character, updating the accumulator.
    -- The accumulator is wrapped in Maybe to handle invalid characters.
    processChar :: Maybe Int -> Char -> Maybe Int
    processChar maybeAcc c = do
      -- Use monadic bind (>>=) to proceed only if maybeAcc is Just
      -- and hexCharToInt returns Just. Otherwise, propagate Nothing.
      acc <- maybeAcc
      digit <- hexCharToInt c
      Just (acc * 16 + digit)

    -- | Converts a single character to its corresponding integer value (0-15).
    -- Returns Nothing if the character is not a valid hexadecimal digit.
    -- Note: digitToInt from Data.Char handles '0'-'9', 'a'-'f', and 'A'-'F'.
    hexCharToInt :: Char -> Maybe Int
    hexCharToInt c
      | isHexDigit c = Just (digitToInt c)
      | otherwise    = Nothing
