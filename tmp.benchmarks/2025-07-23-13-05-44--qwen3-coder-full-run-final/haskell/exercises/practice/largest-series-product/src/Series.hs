module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size == 0 = Right 1
  | any (`notElem` ['0'..'9']) digits = Left (InvalidDigit (head (filter (`notElem` ['0'..'9']) digits)))
  | size > length digits = Left InvalidSpan
  | otherwise = Right (maximum (map product (slidingWindows size digits)))
  where
    slidingWindows n xs = take (length xs - n + 1) (map (take n) (tails xs))
    tails [] = []
    tails xs = xs : tails (tail xs)
    product = foldl (*) 1 . map (fromIntegral . digitToInt)
    digitToInt c = fromEnum c - fromEnum '0'
