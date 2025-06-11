module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isSpace, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = [ toUpper c | (p, c) <- zip (' ':xs) xs, isAlpha c && isAcronymStart p c ]
  where
    isAcronymStart p c = isSpace p || p == '-' || (isLower p && isUpper c)
