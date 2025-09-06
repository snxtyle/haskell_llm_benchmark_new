module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | otherwise = Just $ spellNumber n

spellNumber :: Integer -> String
spellNumber 0 = "zero"
spellNumber n = intercalate " " $ filter (not . null) parts
  where
    parts = map spellChunkWithScale $ zip chunks scales
    chunks = getChunks n
    scales = ["", "thousand", "million", "billion", "trillion"]

getChunks :: Integer -> [Integer]
getChunks 0 = [0]
getChunks n = reverse $ getChunks' n []
  where
    getChunks' 0 acc = acc
    getChunks' x acc = getChunks' (x `div` 1000) ((x `mod` 1000) : acc)

spellChunkWithScale :: (Integer, String) -> String
spellChunkWithScale (0, _) = ""
spellChunkWithScale (n, "") = spellChunk n
spellChunkWithScale (n, scale) = spellChunk n ++ " " ++ scale

spellChunk :: Integer -> String
spellChunk n
  | n < 20 = ones !! fromInteger n
  | n < 100 = 
      let tensPart = tens !! fromInteger (n `div` 10 - 2)
          onesPart = n `mod` 10
      in if onesPart == 0 
         then tensPart
         else tensPart ++ "-" ++ spellChunk onesPart
  | otherwise = spellChunk (n `div` 100) ++ " hundred" ++ rest
  where
    rest = if n `mod` 100 == 0 then "" else " " ++ spellChunk (n `mod` 100)

ones :: [String]
ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
        "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
