module Series (slices) where

import Data.Char (digitToInt)

-- | 与えられた文字列から長さ n の連続部分列をすべて取り出し，
--   各文字を Int に変換したリストを返す。
--   n <= 0 あるいは n > length xs の場合は error を投げる。
slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0            = error "slice length must be positive"
  | n > length xs     = error "slice length exceeds input length"
  | otherwise         = [map digitToInt (take n (drop i xs)) | i <- [0 .. length xs - n]]
