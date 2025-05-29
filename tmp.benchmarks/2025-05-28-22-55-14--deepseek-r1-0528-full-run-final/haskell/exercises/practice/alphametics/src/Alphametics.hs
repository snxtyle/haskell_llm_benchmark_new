module Alphametics (solve) where

import Data.Char (isAlpha)
import Data.List (nub, permutations, (\\))
import Data.Maybe (isNothing, fromJust, listToMaybe)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ [ assign | 
                     perm <- letterPerms,
                     let assign = zip letters perm,
                     all (\(l, d) -> l `notElem` leadingLetters || d /= 0) assign,
                     let sumAddends = map (wordToDecimal assign) addends,
                     let resDec = wordToDecimal assign result,
                     all isValid sumAddends,
                     isValid (Just resDec),
                     sum (map fromJust sumAddends) == fromJust resDec
                   ] where
  isValid (Just n) = n > 0
  isValid Nothing = False

  tokens = words puzzle
  result = last tokens
  addends = take (length tokens - 2) [ token | token <- tokens, token /= "=", token /= "+" ]
  letters = nub (concat addends ++ result)
  leadingLetters = nub $ map head (result:addends)

  letterPerms = permutations (([0,1,2,3,4,5,6,7,8,9] \\ [0]) ++ [0])

  wordToDecimal :: [(Char, Int)] -> String -> Maybe Int
  wordToDecimal assignment word = 
      if any (\c -> isNothing (lookup c assignment)) word then Nothing
      else 
        let digits = map (\c -> fromJust (lookup c assignment)) word
        in 
          if head digits == 0 then Nothing else 
          Just (foldl (\n d -> n * 10 + d) 0 digits)
