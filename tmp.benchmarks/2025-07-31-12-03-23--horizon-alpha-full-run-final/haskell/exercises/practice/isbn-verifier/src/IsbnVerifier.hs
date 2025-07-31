module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn s =
  case traverseChars cleaned of
    Just vals | length vals == 10 ->
      let weights = [10,9..1]
          total = sum (zipWith (*) vals weights)
       in total `mod` 11 == 0
    _ -> False
  where
    cleaned = filter (/= '-') s

    -- Validate and convert characters with rule:
    -- First 9 positions must be digits 0-9.
    -- Last position may be digit 0-9 or 'X' (10).
    traverseChars :: String -> Maybe [Int]
    traverseChars xs
      | length xs /= 10 = Nothing
      | otherwise =
          let (prefix, lastCh) = (take 9 xs, xs !! 9)
          in do
            first9 <- mapM toDigit prefix
            lastVal <- lastCharVal lastCh
            pure (first9 ++ [lastVal])

    toDigit :: Char -> Maybe Int
    toDigit c
      | isDigit c = Just (digitToInt c)
      | otherwise = Nothing

    lastCharVal :: Char -> Maybe Int
    lastCharVal c
      | isDigit c = Just (digitToInt c)
      | c == 'X'  = Just 10
      | otherwise = Nothing
