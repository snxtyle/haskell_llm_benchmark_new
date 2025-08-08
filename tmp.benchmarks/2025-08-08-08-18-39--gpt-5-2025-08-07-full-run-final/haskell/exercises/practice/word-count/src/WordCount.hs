module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs =
  let tokens = tokenize (map toLower xs)
  in map (\grp -> (head grp, length grp)) . group . sort $ tokens

-- Tokenize the input string into words:
-- - Words consist of letters/digits.
-- - Apostrophes are kept only when between alphanumeric characters (contractions).
-- - All other characters separate words.
tokenize :: String -> [String]
tokenize s = reverse (go s [] [])
  where
    flush :: [Char] -> [String] -> [String]
    flush curr acc = if null curr then acc else reverse curr : acc

    go :: String -> [Char] -> [String] -> [String]
    go [] curr acc = flush curr acc
    go (c:rest) curr acc
      | isAlphaNum c = go rest (c:curr) acc
      | c == '\'' =
          case curr of
            [] -> -- Apostrophe at the start of a potential token: treat as separator.
                  go rest curr acc
            _  -> case rest of
                    (n:_) | isAlphaNum n -> -- Apostrophe between alphanum chars: keep it.
                            go rest (c:curr) acc
                    _ -> -- Apostrophe not followed by alphanum: end the current token.
                         go rest [] (flush curr acc)
      | otherwise = -- Any other punctuation/whitespace: end the current token.
          go rest [] (flush curr acc)
