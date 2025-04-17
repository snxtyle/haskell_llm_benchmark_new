module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (unwords)

encode :: String -> String
encode xs =
  let -- 1. Normalize: remove non-alphanum, downcase
      normalized = [ toLower c | c <- xs, isAlphaNum c ]

      -- 2. Determine rectangle size
      len      = length normalized
      root     = sqrt (fromIntegral len :: Double)
      r0       = floor root
      c0       = ceiling root
      rows     = if r0 * c0 < len then r0 + 1 else r0
      cols     = c0

      -- 3. Pad to fill the rectangle
      padded   = normalized ++ replicate (rows * cols - len) ' '

      -- 4. Read down the columns to form each chunk
      chunks   = [ [ padded !! (j * cols + i) | j <- [0 .. rows - 1] ]
                 | i <- [0 .. cols - 1]
                 ]
  in unwords chunks
