module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

-- | Count how many times each word occurs in the input text.
--   Words are case–insensitive sequences of letters or digits that may
--   contain an internal apostrophe (e.g. "don't").
wordCount :: String -> [(String, Int)]
wordCount =
  map (\g -> (head g, length g))      -- turn each group into (word, count)
  . group
  . sort                              -- grouping needs sorted input
  . extractWords

-------------------------------------------------------------------------------
-- Word extraction
-------------------------------------------------------------------------------

-- | Extract all words from a subtitle according to the rules in the exercise.
extractWords :: String -> [String]
extractWords = scan []               -- current-buffer (reversed)
  where
    -- Recursively scan the remaining characters.
    scan :: String -> String -> [String]
    scan buf [] =
      finish buf                     -- end of input
    scan buf (c:cs)
      -- Alphanumeric characters are always part of the current word.
      | isAlphaNum c =
          scan (toLower c : buf) cs

      -- Apostrophe kept only when it sits *between* two alphanumerics.
      | c == '\'' && isInternalApostrophe buf cs =
          scan (c : buf) cs

      -- Separator met while buffer empty: just skip it.
      | null buf =
          scan [] cs

      -- Separator met with non‑empty buffer: finish current word.
      | otherwise =
          finish buf ++ scan [] cs

    -- Flush the accumulated buffer (if any) into a singleton list.
    finish :: String -> [String]
    finish [] = []
    finish b  = [reverse b]

    -- Check if the apostrophe is between two alphanumerics:
    --   we have at least one char already (prev) *and*
    --   the next char is alphanumeric.
    isInternalApostrophe :: String -> String -> Bool
    isInternalApostrophe prev next =
      not (null prev) && case next of
        d : _ -> isAlphaNum d
        _     -> False
