module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct span digits
  | span < 0 = Left InvalidSpan
  | span > length digits = Left InvalidSpan
  | otherwise = case validateDigits digits of
      Just c -> Left (InvalidDigit c)
      Nothing -> Right $ if span == 0
                         then 1
                         else maximum $ map productOfSeries (getSeries span digits)
  where
    -- Check if all characters are digits, return first non-digit if found
    validateDigits :: String -> Maybe Char
    validateDigits [] = Nothing
    validateDigits (c:cs)
      | c >= '0' && c <= '9' = validateDigits cs
      | otherwise = Just c
    
    -- Get all consecutive series of given span from the string
    getSeries :: Int -> String -> [String]
    getSeries n str
      | length str < n = []
      | otherwise = take n str : getSeries n (tail str)
    
    -- Calculate the product of digits in a series
    productOfSeries :: String -> Integer
    productOfSeries = product . map charToInteger
    
    -- Convert a digit character to Integer
    charToInteger :: Char -> Integer
    charToInteger c = toInteger (fromEnum c - fromEnum '0')
