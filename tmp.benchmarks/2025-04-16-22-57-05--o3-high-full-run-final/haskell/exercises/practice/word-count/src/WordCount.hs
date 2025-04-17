module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

-- | Count the number of occurrences of each word in a piece of text.
--
-- A “word” is:
--   * a consecutive sequence of letters / digits,
--   * that may contain a single apostrophe **inside** the sequence
--     (to keep contractions such as “they're” together).
--
-- Everything else (punctuation, whitespace, leading/trailing apostrophes,
-- etc.) acts as a separator.
--
-- The match is case‑insensitive and numbers are treated as words.
--
-- The order of the resulting list is unspecified.
wordCount :: String -> [(String, Int)]
wordCount =
    map (\ws -> (head ws, length ws))  -- pack each group into (word,count)
  . group                              -- group identical neighbouring words
  . sort                               -- group/merge needs sorted input
  . tokenize                           -- break the input into words
  where
    -- | Split the input into lowercase words following the rules above.
    tokenize :: String -> [String]
    tokenize text = reverse (go text [] [])
      where
        -- go remainingChars currentWord accumulator
        go :: String -> String -> [String] -> [String]
        go [] curr acc = addCurrent curr acc
        go (c:cs) curr acc
          | isAlphaNum c
              = go cs (toLower c : curr) acc
          | c == '\''
              -- Keep apostrophe only when it sits *inside* a word,
              -- i.e. we already have some characters and the next one
              -- is alphanumeric.
              = if not (null curr) && not (null cs) && isAlphaNum (head cs)
                   then go cs (c : curr) acc            -- keep it
                   else go cs [] (addCurrent curr acc)  -- treat as separator
          | otherwise
              = go cs [] (addCurrent curr acc)          -- separator

        -- Push the current (reversed) word into the accumulator
        addCurrent :: String -> [String] -> [String]
        addCurrent [] acc   = acc
        addCurrent curr acc = reverse curr : acc
