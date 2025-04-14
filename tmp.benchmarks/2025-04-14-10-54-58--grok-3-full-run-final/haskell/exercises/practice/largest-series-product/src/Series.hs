module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | size < 0 = Left InvalidSpan
    | size > length digits = Left InvalidSpan
    | size == 0 = Right 1
    | not (all (`elem` "0123456789") digits) = Left (InvalidDigit (head (filter (`notElem` "0123456789") digits)))
    | otherwise = Right (maximum (map product (series size digits)))
    where
        series n xs = map (take n) (tails n xs)
        tails n xs
            | length xs < n = []
            | otherwise = xs : tails n (tail xs)
        product xs = foldl (*) 1 (map (fromIntegral . digitToInt) xs)
        digitToInt c = fromEnum c - fromEnum '0'
