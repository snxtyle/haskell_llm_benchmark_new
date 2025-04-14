module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn s =
  let
    -- Remove hyphens
    filtered = filter (/= '-') s

    -- Check length
    validLength = length filtered == 10

    -- Convert characters to values, last char can be 'X' meaning 10
    charValues :: [Maybe Int]
    charValues = map charToValue filtered

    charToValue :: Char -> Maybe Int
    charToValue c
      | isDigit c = Just (digitToInt c)
      | c == 'X' = Just 10
      | otherwise = Nothing

    -- Check that 'X' only appears as the last character
    xOnlyAtEnd = not ('X' `elem` init filtered)

    -- Calculate weighted sum if all chars valid and 'X' only at end
    weightedSum :: Maybe Int
    weightedSum = if validLength && all (/= Nothing) charValues && xOnlyAtEnd
                  then Just $ sum $ zipWith (*) (map (\(Just x) -> x) charValues) [10,9..1]
                  else Nothing
  in
    case weightedSum of
      Just s -> s `mod` 11 == 0
      Nothing -> False
