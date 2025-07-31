module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)

-- Map a single character using Atbash rules:
-- - letters: map a..z <-> z..a (case-insensitive)
-- - digits: kept as-is
-- - other characters: ignored (return Nothing)
mapChar :: Char -> Maybe Char
mapChar c
  | isAlpha c = Just (atbashLower (toLower c))
  | isDigit c = Just c
  | otherwise = Nothing
  where
    atbashLower :: Char -> Char
    atbashLower ch =
      -- 'a' -> 'z', 'b' -> 'y', ..., 'z' -> 'a'
      chr (ord 'a' + ord 'z' - ord ch)

-- Helper to process a string into a stream of mapped chars (letters/digits only)
mappedStream :: String -> String
mappedStream = foldr step []
  where
    step ch acc = case mapChar ch of
      Just m  -> m : acc
      Nothing -> acc

-- Group a string into chunks of n characters, separated by spaces
groupChunks :: Int -> String -> String
groupChunks n = unwords . chunks n
  where
    chunks _ [] = []
    chunks k xs = let (a, b) = splitAt k xs in a : chunks k b

encode :: String -> String
encode = groupChunks 5 . mappedStream

decode :: String -> String
decode = mappedStream
