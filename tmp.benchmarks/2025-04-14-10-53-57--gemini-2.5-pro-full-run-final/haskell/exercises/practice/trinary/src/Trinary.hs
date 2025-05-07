module Trinary (readTri, showTri) where

-- Use foldl' for strictness as hinted
import Prelude hiding (foldl)
import Data.List (foldl')

-- Helper function to convert a character to its integer value if it's a valid trinary digit.
-- Returns Nothing for invalid characters.
-- Using Integer as the tests expect readTri to return Integer.
digitToInt :: Char -> Maybe Integer
digitToInt '0' = Just 0
digitToInt '1' = Just 1
digitToInt '2' = Just 2
digitToInt _   = Nothing

-- | Converts a string representation of a trinary number to its decimal integer value.
-- Returns 0 if the string contains any invalid characters.
-- Changed return type to Integer to match test expectations.
readTri :: String -> Integer
readTri s = case foldl' step (Just 0) s of
              Nothing  -> 0 -- If the fold resulted in Nothing (invalid input), return 0.
              Just val -> val -- Otherwise, return the calculated value.
  where
    -- The step function for the strict left fold (foldl').
    -- Takes the accumulator (Maybe Integer) and the next character.
    -- Returns Nothing if an invalid state is reached or an invalid character is encountered.
    step :: Maybe Integer -> Char -> Maybe Integer
    step Nothing _ = Nothing -- If already invalid, propagate Nothing.
    step (Just acc) c =
      case digitToInt c of
        Nothing -> Nothing -- Current character is invalid, result becomes Nothing.
        Just d  -> Just $! (acc * 3 + d) -- Valid digit, update accumulator strictly.

-- Add type signature to silence warning.
-- The type matches what the test suite seems to imply (converting an Integer to a String).
showTri :: Integer -> String
showTri = error "You need to implement this function."
