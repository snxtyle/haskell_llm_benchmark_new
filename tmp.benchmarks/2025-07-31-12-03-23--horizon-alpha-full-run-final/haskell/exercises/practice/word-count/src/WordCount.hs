module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import qualified Data.Map.Strict as M

wordCount :: String -> [(String, Int)]
wordCount = M.toList . foldr (\w -> M.insertWith (+) w 1) M.empty . tokenize . map toLower

-- Tokenize into words where letters/digits are word chars and
-- apostrophes are allowed only inside a word (not leading or trailing).
tokenize :: String -> [String]
tokenize = go [] []
  where
    -- go acc current s
    -- acc: reversed list of completed tokens
    -- current: reversed chars of current token
    go acc cur [] =
      let acc' = case finalize cur of
                   Just w  -> w : acc
                   Nothing -> acc
      in reverse acc'
    go acc cur (c:cs)
      | isWordChar c =
          go acc (c:cur) cs
      | c == '\'' =
          -- Allow apostrophe only if we already have at least one word char
          -- and the next char is a word char; otherwise treat as separator.
          case cs of
            n:_ | not (null cur) && isWordChar n ->
                    go acc (c:cur) cs
            _ -> -- treat as separator; finalize current token
                 let acc' = case finalize cur of
                              Just w  -> w : acc
                              Nothing -> acc
                 in go acc' [] cs
      | otherwise =
          -- separator: finalize current token
          let acc' = case finalize cur of
                       Just w  -> w : acc
                       Nothing -> acc
          in go acc' [] cs

    isWordChar :: Char -> Bool
    isWordChar = isAlphaNum

    -- Finalize a token: current is reversed
    -- Reject tokens that start or end with apostrophe (after reversing),
    -- and collapse internal apostrophes as-is.
    finalize :: [Char] -> Maybe String
    finalize [] = Nothing
    finalize revCur =
      let w = reverse revCur
      in if head w == '\'' || last w == '\''
            then Nothing
            else Just w
