module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

-- | Validate whether a given string is a correct ISBN‑10.
--   The input may contain hyphens.  The function
--   returns True only when, after stripping hyphens,
--   the string
--     * has exactly 10 characters,
--     * the first 9 are digits,
--     * the last is either a digit or the letter 'X',
--       where 'X' represents the value 10,
--     * and the weighted checksum equals 0 modulo 11.
isbn :: String -> Bool
isbn raw =
  let cleaned = filter (/= '-') raw
  in  case cleaned of
        -- must be exactly 10 characters after removing hyphens
        _ | length cleaned /= 10 -> False
        -- split into first 9 characters and the check character
        _ ->
          let (body, checkCharStr) = splitAt 9 cleaned
          in  case checkCharStr of
                [checkChar] ->
                  all isDigit body
                  && (isDigit checkChar || checkChar == 'X')
                  && checksum (map charValue (body ++ [checkChar])) `mod` 11 == 0
                _ -> False
  where
    -- Convert a character to its numeric value
    charValue :: Char -> Int
    charValue 'X' = 10
    charValue c   = digitToInt c

    -- Calculate checksum using weights 10 down to 1
    checksum :: [Int] -> Int
    checksum digits = sum $ zipWith (*) digits [10,9..1]
