module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (convertChunks (chunkify n))

chunkify :: Integer -> [Integer]
chunkify 0 = []
chunkify n = (n `mod` 1000) : chunkify (n `div` 1000)

scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"]

convertChunks :: [Integer] -> String
convertChunks chunks =
    let chunkScalePairs = zip chunks scales
        convertedPairs = map (\(c, s) ->
            if c == 0
            then ""
            else toEnglishUnder1000 c ++ if null s then "" else " " ++ s
            ) chunkScalePairs
        nonEmptyParts = filter (not . null) convertedPairs
    in unwords (reverse nonEmptyParts)

toEnglishUnder1000 :: Integer -> String
toEnglishUnder1000 n
    | n < 100 = toEnglishUnder100 n
    | otherwise =
        let hundreds = n `div` 100
            remainder = n `mod` 100
        in if remainder == 0
           then toEnglishUnder20 hundreds ++ " hundred"
           else toEnglishUnder20 hundreds ++ " hundred " ++ toEnglishUnder100 remainder

toEnglishUnder100 :: Integer -> String
toEnglishUnder100 n
    | n < 20 = toEnglishUnder20 n
    | otherwise =
        let tens = n `div` 10
            units = n `mod` 10
        in if units == 0
           then toEnglishTens tens
           else toEnglishTens tens ++ "-" ++ toEnglishUnder20 units

toEnglishUnder20 :: Integer -> String
toEnglishUnder20 n = case n of
    0  -> "zero"
    1  -> "one"
    2  -> "two"
    3  -> "three"
    4  -> "four"
    5  -> "five"
    6  -> "six"
    7  -> "seven"
    8  -> "eight"
    9  -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
    _  -> error "out of range for toEnglishUnder20"

toEnglishTens :: Integer -> String
toEnglishTens n = case n of
    2 -> "twenty"
    3 -> "thirty"
    4 -> "forty"
    5 -> "fifty"
    6 -> "sixty"
    7 -> "seventy"
    8 -> "eighty"
    9 -> "ninety"
    _ -> error "out of range for toEnglishTens"
