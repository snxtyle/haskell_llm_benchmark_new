module Alphametics (solve) where

import Data.Char (isAlpha)
import Data.List (words, nub, (\\))
import Data.Maybe (fromJust, listToMaybe, catMaybes)

solve :: String -> Maybe [(Char, Int)]
solve puzzle
  | length allWords < 2 = Nothing  -- Invalid puzzle: need at least one addend and a sum
  | otherwise = backtrack letters []  -- Start backtracking with empty mapping
  where
    parts = words puzzle  -- Split into words, e.g., ["SEND", "+", "MORE", "==", "MONEY"]
    allWords = filter (all isAlpha) parts  -- Filter to actual words: ["SEND", "MORE", "MONEY"]
    sumWord = last allWords  -- e.g., "MONEY"
    addends = init allWords  -- e.g., ["SEND", "MORE"]
    letters = nub $ concat allWords  -- Unique letters, e.g., ['S','E','N','D','M','O','R','Y']
    firstLetters = nub $ map head (filter ((>1) . length) allWords)  -- First letters of multi-digit words, e.g., ['S','M']

    backtrack :: [Char] -> [(Char, Int)] -> Maybe [(Char, Int)]
    backtrack [] currentMapping =  -- All letters assigned
      let addendInts = map (wordToInt currentMapping) addends
          sumInt = wordToInt currentMapping sumWord
      in if sum addendInts == sumInt then Just currentMapping else Nothing
    
    backtrack (letter:rest) currentMapping =
      let availableDigits = [0..9] \\ map snd currentMapping  -- Digits not yet used
          possibleDigits = if letter `elem` firstLetters then filter (/= 0) availableDigits else availableDigits  -- Exclude 0 for first letters
      in listToMaybe $ catMaybes [ backtrack rest ((letter, digit):currentMapping) | digit <- possibleDigits ]

    wordToInt :: [(Char, Int)] -> String -> Int
    wordToInt mapping word =
      foldl (\acc c -> acc * 10 + fromJust (lookup c mapping)) 0 word  -- Convert word to integer using mapping
