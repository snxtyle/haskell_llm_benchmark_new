module Acronym (abbreviate) where

import Data.Char (isAlpha, isSpace, toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate xs = [ toUpper (head w) | w <- words (splitCamelCase (cleanString xs)), not (null w) ]
  where
    cleanString s = [ if c == '-' then ' ' else c | c <- s, isAlpha c || isSpace c || c == '-' ]
    splitCamelCase s = snd $ foldl f (False, "") s
      where
        f (prevLower, acc) c = (isLower c, acc ++ if isUpper c && prevLower then " " ++ [c] else [c])
