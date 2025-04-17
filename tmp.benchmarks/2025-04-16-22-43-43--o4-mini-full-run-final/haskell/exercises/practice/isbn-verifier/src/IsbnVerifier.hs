module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn input =
  let clean = filter (/= '-') input
  in case clean of
       xs | length xs == 10 ->
            case parseDigits xs of
              Just digits -> sum (zipWith (*) digits [10,9..1]) `mod` 11 == 0
              Nothing     -> False
          | otherwise -> False

-- Parse the first 9 characters as digits 0-9 and the last as 0-10 (X -> 10)
parseDigits :: String -> Maybe [Int]
parseDigits xs = do
  let (prefix, [c10]) = splitAt 9 xs
  ds  <- mapM parseDigit prefix
  d10 <- parseCheckDigit c10
  return (ds ++ [d10])

parseDigit :: Char -> Maybe Int
parseDigit c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

parseCheckDigit :: Char -> Maybe Int
parseCheckDigit c
  | isDigit c = Just (digitToInt c)
  | c == 'X'  = Just 10
  | otherwise = Nothing
