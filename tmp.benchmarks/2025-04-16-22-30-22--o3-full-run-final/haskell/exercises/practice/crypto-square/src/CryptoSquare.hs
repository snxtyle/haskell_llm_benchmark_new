module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- | Encode a piece of text using the crypto‑square cipher.
--
-- The algorithm follows these steps:
--
-- 1. Normalise – keep only alpha‑numeric characters and convert everything
--    to lower‑case.
-- 2. Determine the rectangle dimensions (rows `r` and columns `c`) such that
--        • r * c >= length
--        • c ≥ r
--        • c - r ≤ 1
--    with the smallest possible `c`.
-- 3. Write the text into the rectangle row‑wise, padding the last row with
--    spaces.
-- 4. Read the rectangle column‑wise, producing `c` chunks, each of length `r`.
--    Chunks are separated by a single space.  Inside every chunk trailing
--    spaces are preserved so that each chunk is exactly `r` characters long.
encode :: String -> String
encode xs
  | null normalized = ""
  | otherwise       = unwords columnChunks
  where
    normalized = map toLower . filter isAlphaNum $ xs
    len        = length normalized
    (r, c)     = rectangle len

    -- Split the normalised text into rows of length `c`
    rows       = chunksOf c normalized
    -- Pad each row with spaces so that it has exactly `c` characters
    paddedRows = map (padRight c ' ') rows
                 ++ replicate (r - length rows) (replicate c ' ')

    -- Read column‑wise to create the encoded chunks
    columnChunks = map (take r) . transpose $ paddedRows

-- | Compute the rectangle dimensions (rows, columns) for the given length.
rectangle :: Int -> (Int, Int)
rectangle n =
  let r0 = floor  (sqrt (fromIntegral n :: Double))
      c0 = ceiling (sqrt (fromIntegral n :: Double))
      r  = if r0 * c0 < n then r0 + 1 else r0
      c  = c0
  in (r, c)

-- | Split a list into chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n ys = first : chunksOf n rest
  where
    (first, rest) = splitAt n ys

-- | Pad the list on the right with the given element until it reaches
--   the desired length.
padRight :: Int -> a -> [a] -> [a]
padRight n pad xs = xs ++ replicate (n - length xs) pad
