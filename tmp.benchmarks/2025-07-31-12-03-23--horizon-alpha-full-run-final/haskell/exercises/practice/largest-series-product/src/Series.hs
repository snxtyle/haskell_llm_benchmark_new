module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size == 0 = Right 1
  | size > length digits = Left InvalidSpan
  | otherwise = do
      -- Validate all digits first
      mapM_ validateDigit digits
      let windows = consecutive size digits
          products = map productOfDigits windows
      case products of
        [] -> Right 1 -- This case won't occur due to earlier guards, but kept for completeness
        _  -> Right (maximum products)
  where
    validateDigit c
      | isDigit c = Right ()
      | otherwise = Left (InvalidDigit c)

    consecutive n xs = takeWhile ((== n) . length) $ map (take n) (tails xs)

    productOfDigits :: String -> Integer
    productOfDigits = foldr (\c acc -> toInteger (digitToInt c) * acc) 1
