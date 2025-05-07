module Say (inEnglish) where

import Data.List (intercalate)
-- Removed unused import Data.Maybe

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ sayNumber n

sayNumber :: Integer -> String
sayNumber n
  | n < 100 = sayDoubleDigit n
  | otherwise = intercalate " " $ filter (not . null) $ combineChunks $ zip (reverse (splitByThousands n)) scales
  where
    scales = ["", "thousand", "million", "billion", "trillion"]

combineChunks :: [(Integer, String)] -> [String]
combineChunks = foldl f []
  where
    f acc (0, _) = acc
    f acc (num, scale)
      | null scale = acc ++ [sayTripleDigit num]
      | otherwise  = acc ++ [sayTripleDigit num ++ " " ++ scale]

splitByThousands :: Integer -> [Integer]
splitByThousands 0 = []
splitByThousands n = let (q, r) = n `divMod` 1000 in r : splitByThousands q

sayDoubleDigit :: Integer -> String
sayDoubleDigit n
  | n < 10 = units !! fromIntegral n
  | n < 20 = teens !! fromIntegral (n - 10)
  | n < 100 =
      let (t, u) = n `divMod` 10
          tensPart = tens !! fromIntegral t
          unitsPart = if u == 0 then "" else "-" ++ units !! fromIntegral u
      in tensPart ++ unitsPart
  | otherwise = error "sayDoubleDigit only supports numbers < 100"
  where
    units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens  = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

sayTripleDigit :: Integer -> String
sayTripleDigit n
  | n < 100 = sayDoubleDigit n
  | otherwise =
      let (h, rest) = n `divMod` 100
          hundredsPart = units !! fromIntegral h ++ " hundred"
          restPart = if rest == 0 then "" else " " ++ sayDoubleDigit rest
      in hundredsPart ++ restPart
  where
    units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
