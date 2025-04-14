module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ go xs True
  where
    go [] _ = []
    go (c:cs) prevIsSeparator
      | c == '-' = go cs True
      | isAlpha c && prevIsSeparator = c : go cs False
      | isUpper c = c : go cs False
      | otherwise = go cs False
