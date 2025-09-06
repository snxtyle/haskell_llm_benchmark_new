module Say (inEnglish) where

import Data.List (intercalate, unfoldr)

units :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"]

spell :: Integer -> String
spell 0 = ""
spell n = let hundreds = n `div` 100
              rem = n `mod` 100
              h = if hundreds > 0 then units !! fromIntegral hundreds ++ " hundred" else ""
              t = spellTens rem
          in if h == "" then t else h ++ (if t == "" then "" else " " ++ t)

spellTens :: Integer -> String
spellTens 0 = ""
spellTens n | n < 10 = units !! fromIntegral n
            | n < 20 = teens !! fromIntegral (n - 10)
            | otherwise = let (t, u) = n `divMod` 10
                          in tens !! fromIntegral t ++ (if u == 0 then "" else "-" ++ units !! fromIntegral u)

combine :: Integer -> String -> String
combine chunk scale = if chunk == 0 then "" else spell chunk ++ (if scale == "" then "" else " " ++ scale)

process :: Integer -> String
process 0 = "zero"
process n = let gs = unfoldr (\x -> if x == 0 then Nothing else let (q, r) = x `divMod` 1000 in Just (r, q)) n
            in intercalate " " . filter (not . null) . reverse $ zipWith combine gs scales

inEnglish :: Integer -> Maybe String
inEnglish n | n < 0 || n > 999999999999 = Nothing
            | otherwise = Just (process n)
