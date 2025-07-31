module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs =
  let normalized = normalize xs
      len = length normalized
  in if len == 0
       then ""
       else
         let (r, c) = dims len
             rows = chunkPad c ' ' normalized
             -- Transpose converts rows (r rows of length c)
             -- into columns (c rows of length r)
             columns = transpose rows
         in unwords columns

-- Normalize the input: keep only alphanumeric characters and lowercase them.
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Determine rows (r) and columns (c) complying with:
-- r * c >= n, c >= r, and c - r <= 1, minimizing c.
dims :: Int -> (Int, Int)
dims n = head
  [ (r, c)
  | c <- [1..]
  , let r = ceiling (fromIntegral n / fromIntegral c)
  , c >= r
  , c - r <= 1
  , r * c >= n
  ]

-- Break the string into rows of width w, padding the last row with pad
-- so that we have exactly r rows of length w, where r = ceiling (len / w).
chunkPad :: Int -> Char -> String -> [String]
chunkPad w padChar s =
  let chunks = go s
      r = ceiling (fromIntegral (length s) / fromIntegral w)
      -- Ensure we have exactly r rows; if s was empty, r may be 0.
      rows = take r (chunks ++ repeat (replicate w padChar))
  in rows
  where
    go "" = []
    go str =
      let (a, b) = splitAt w str
      in if length a == w
           then a : go b
           else [a ++ replicate (w - length a) padChar]
