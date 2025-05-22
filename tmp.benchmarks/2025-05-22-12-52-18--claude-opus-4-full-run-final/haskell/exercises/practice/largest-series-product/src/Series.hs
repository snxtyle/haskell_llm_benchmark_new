module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct span digits
  | span < 0 = Left InvalidSpan
  | span > length digits = Left InvalidSpan
  | span == 0 = Right 1
  | otherwise = case validateDigits digits of
      Just c -> Left (InvalidDigit c)
      Nothing -> Right $ maximum $ map productOfSeries $ series span digits
  where
    validateDigits :: String -> Maybe Char
    validateDigits [] = Nothing
    validateDigits (c:cs)
      | c >= '0' && c <= '9' = validateDigits cs
      | otherwise = Just c
    
    series :: Int -> String -> [String]
    series n str
      | length str < n = []
      | otherwise = take n str : series n (tail str)
    
    productOfSeries :: String -> Integer
    productOfSeries = product . map (toInteger . digitToInt)
    
    digitToInt :: Char -> Int
    digitToInt c = fromEnum c - fromEnum '0'
