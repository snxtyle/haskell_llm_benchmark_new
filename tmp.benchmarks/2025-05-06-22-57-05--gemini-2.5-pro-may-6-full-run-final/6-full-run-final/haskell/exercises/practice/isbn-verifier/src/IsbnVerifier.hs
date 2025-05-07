module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

-- | Checks if a given string is a valid ISBN-10 number.
isbn :: String -> Bool
isbn s =
    let cleaned = filter (/= '-') s
    in case parseDigits cleaned of
        Just digits -> length digits == 10 && (sum (zipWith (*) digits [10,9..1]) `mod` 11 == 0)
        Nothing     -> False

-- | Parses a cleaned ISBN string (no hyphens) into a list of integers.
-- Returns Nothing if the string is not 10 characters long, or if it contains invalid characters,
-- or if 'X' appears in a position other than the last one.
parseDigits :: String -> Maybe [Int]
parseDigits s
    | length s /= 10 = Nothing
    | otherwise      = parseChars (init s) (last s)

-- | Helper function to parse the first nine characters and the last (check) character.
-- It uses mapM to ensure all first nine characters are valid digits.
parseChars :: String         -- ^ The first nine characters
           -> Char           -- ^ The last (check) character
           -> Maybe [Int]    -- ^ Just list of 10 integers if valid, Nothing otherwise
parseChars firstNineChars lastChar =
    case mapM parseNumericChar firstNineChars of
        Nothing      -> Nothing
        Just nineInts -> case parseCheckChar lastChar of
            Nothing     -> Nothing
            Just lastInt -> Just (nineInts ++ [lastInt])

-- | Parses a character that is expected to be a digit.
parseNumericChar :: Char -> Maybe Int
parseNumericChar c
    | isDigit c = Just (digitToInt c)
    | otherwise = Nothing

-- | Parses the check character, which can be a digit or 'X'.
parseCheckChar :: Char -> Maybe Int
parseCheckChar c
    | isDigit c = Just (digitToInt c)
    | c == 'X'  = Just 10
    | otherwise = Nothing
