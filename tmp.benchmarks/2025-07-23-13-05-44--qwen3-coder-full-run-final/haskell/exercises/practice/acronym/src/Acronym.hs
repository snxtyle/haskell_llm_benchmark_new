module Acronym (abbreviate) where

import Data.Char (toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpperFirst (words (normalizeInput xs))
  where
    normalizeInput = filter (/= '\'') . map replaceHyphen
    replaceHyphen '-' = ' '
    replaceHyphen c = c
    toUpperFirst [] = error "Empty word"
    toUpperFirst (c:_) = toUpper c
