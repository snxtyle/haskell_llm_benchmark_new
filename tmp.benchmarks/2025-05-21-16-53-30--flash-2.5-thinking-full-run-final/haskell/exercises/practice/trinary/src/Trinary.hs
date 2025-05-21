module Trinary (readTri, showTri) where

-- Helper function to convert a character to its trinary digit value.
-- Returns Nothing if the character is not '0', '1', or '2'.
charToDigit :: Char -> Maybe Int
charToDigit c
  | c == '0' = Just 0
  | c == '1' = Just 1
  | c == '2' = Just 2
  | otherwise = Nothing

-- | Converts a trinary string to its decimal equivalent.
--   Returns 0 if the string contains any invalid trinary digits.
readTri :: String -> Int
readTri s = go 0 s
  where
    -- The accumulator 'acc' is strict due to the bang pattern '!'
    -- This ensures that the accumulator is evaluated at each step,
    -- preventing the build-up of thunks and improving efficiency.
    go :: Int -> String -> Int
    go !acc [] = acc -- Base case: end of string, return accumulated value
    go !acc (c:cs) =
      case charToDigit c of
        Just d -> go (acc * 3 + d) cs -- Valid digit: update accumulator and recurse
        Nothing -> 0                 -- Invalid character found, return 0 immediately

-- | Converts a decimal number to its trinary string representation.
--   (Implementation not provided by the problem description)
showTri :: Int -> String
showTri = error "You need to implement this function."
