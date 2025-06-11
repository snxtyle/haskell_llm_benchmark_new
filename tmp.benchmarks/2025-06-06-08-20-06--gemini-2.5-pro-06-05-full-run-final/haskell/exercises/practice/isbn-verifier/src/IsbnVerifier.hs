module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)
import Data.Traversable (traverse)

isbn :: String -> Bool
isbn s =
  case parseIsbn s of
    Nothing -> False
    Just digits ->
      let multipliers = [10, 9..1]
          total = sum $ zipWith (*) digits multipliers
      in total `mod` 11 == 0
  where
    parseIsbn :: String -> Maybe [Int]
    parseIsbn str =
      let cleaned = filter (/= '-') str
      in if length cleaned /= 10
         then Nothing
         else do
           let body = take 9 cleaned
           let check = last cleaned
           bodyDigits <- traverse charToDigit body
           checkDigit <- checkCharToDigit check
           return (bodyDigits ++ [checkDigit])

    charToDigit :: Char -> Maybe Int
    charToDigit c
      | isDigit c = Just (digitToInt c)
      | otherwise = Nothing

    checkCharToDigit :: Char -> Maybe Int
    checkCharToDigit c
      | isDigit c = Just (digitToInt c)
      | c == 'X'  = Just 10
      | otherwise = Nothing
