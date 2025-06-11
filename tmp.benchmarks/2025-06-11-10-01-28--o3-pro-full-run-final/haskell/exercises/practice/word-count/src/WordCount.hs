module WordCount (wordCount) where

import Data.Char       (isAlphaNum, toLower)
import Data.List       (group, sort)

-- | Count how many times each word appears in a subtitle.
--
-- A *word* is:
--   * A sequence of ASCII letters or digits.
--   * Apostrophes (') **inside** such a sequence are kept (to handle
--     contractions like “don't” or “it's”).
--   * Apostrophes used as opening/closing quotes are ignored.
-- Words are compared case-insensitively.
--
-- The order of the resulting list is unspecified.
wordCount :: String -> [(String, Int)]
wordCount =
    map toPair
  . group                -- group identical words
  . sort                 -- … after sorting them
  . map (map toLower)    -- case-insensitive comparison
  . filter (not . null)  -- remove empty tokens
  . map stripQuotes      -- strip leading/trailing apostrophes
  . splitWords           -- tokenise input
  where
    toPair g = (head g, length g)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Split a string into raw tokens where characters that form part of
-- a word are kept together.  Word characters are ASCII letters,
-- digits, or apostrophes.
splitWords :: String -> [String]
splitWords = go []
  where
    go acc []       = finish acc
    go acc (c:cs)
      | isWordChar c = go (c:acc) cs               -- continue current token
      | otherwise    = finish acc ++ go [] cs      -- separator found
    -- finish the current token (if any) and reset the accumulator
    finish [] = []
    finish xs = [reverse xs]

    isWordChar ch = isAlphaNum ch || ch == '\''

-- | Remove apostrophes occurring at the beginning or end of a token.
-- Apostrophes inside the token (e.g. in “can't”) are kept.
stripQuotes :: String -> String
stripQuotes = dropWhileEnd' (== '\'') . dropWhile (== '\'')
  where
    dropWhileEnd' p = reverse . dropWhile p . reverse
