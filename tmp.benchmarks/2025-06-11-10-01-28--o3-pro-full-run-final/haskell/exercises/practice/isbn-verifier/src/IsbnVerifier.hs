module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

-- | Validate an ISBN-10 string.
--
--   The function ignores hyphens, checks that after removing hyphens
--   exactly ten characters remain, and then validates them using the
--   ISBN-10 checksum formula:
--
--   (d₁ * 10 + d₂ * 9 + … + d₁₀ * 1) `mod` 11 == 0
--
--   The last character may be the letter 'X', representing the value 10.
--   Any other non-digit character, or an 'X' in any position other than
--   the last, makes the ISBN invalid.
isbn :: String -> Bool
isbn input =
  case traverseCharValues cleaned of
    Nothing      -> False
    Just digits  -> checksum digits
  where
    -- Remove hyphens.
    cleaned = filter (/= '-') input

    -- Convert the cleaned characters into their numeric values,
    -- returning Nothing if the characters are invalid.
    traverseCharValues :: String -> Maybe [Int]
    traverseCharValues str
      | length str /= 10 = Nothing
      | otherwise        = sequence $ zipWith charToValue [10,9..1] str

    -- Convert a single character to its numeric contribution.
    charToValue :: Int -> Char -> Maybe Int
    charToValue weight c
      | c == 'X'  = if weight == 1 then Just 10 else Nothing
      | isDigit c = Just (digitToInt c)
      | otherwise = Nothing

    -- Apply the ISBN-10 checksum formula.
    checksum :: [Int] -> Bool
    checksum ds = (sum (zipWith (*) ds [10,9..1])) `mod` 11 == 0
