module Acronym (abbreviate) where

import Data.Char (isAlpha, isSpace, toUpper, isUpper)

abbreviate :: String -> String
abbreviate s =
  let -- Treat hyphens like spaces
      s1 = map (\c -> if c == '-' then ' ' else c) s
      -- Drop all other non-letters (turn them into spaces)
      s2 = map (\c -> if isAlpha c || isSpace c then c else ' ') s1
      -- Split on whitespace
      ws = words s2
      -- For each word, further split at camel-case boundaries and take the first letter
      initials = [ toUpper (head part)
                 | w <- ws
                 , part <- splitCamel w
                 , not (null part)
                 ]
   in initials

-- Split a single word at each uppercase letter (preserving the uppercase letter at start of each piece)
splitCamel :: String -> [String]
splitCamel = finalize . foldl splitter ([], "")
  where
    splitter :: ([String], String) -> Char -> ([String], String)
    splitter (subs, curr) c
      | isUpper c
      = if null curr
          then (subs, curr ++ [c])    -- still the first chunk
          else (subs ++ [curr], [c]) -- start a new chunk
      | otherwise
      = (subs, curr ++ [c])         -- continue current chunk

    finalize (subs, curr)
      | null curr = subs
      | otherwise = subs ++ [curr]
