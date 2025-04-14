module Binary (toDecimal) where

import Data.Maybe (fromMaybe)

-- | Converts a binary string representation to its decimal equivalent.
-- Returns 0 if the input string contains invalid characters (not '0' or '1').
toDecimal :: String -> Int
toDecimal = fromMaybe 0 . foldl step (Just 0)
  where
    -- Helper function for the fold.
    -- Takes the accumulated Maybe Int and the next character.
    -- Returns Nothing if the accumulator is already Nothing or the character is invalid.
    -- Otherwise, updates the accumulator with the new decimal value.
    step :: Maybe Int -> Char -> Maybe Int
    step Nothing _ = Nothing -- Propagate failure
    step (Just acc) c
      | c == '0' = Just (acc * 2 + 0)
      | c == '1' = Just (acc * 2 + 1)
      | otherwise = Nothing -- Invalid character encountered
