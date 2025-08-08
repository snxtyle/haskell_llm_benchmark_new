module Trinary (readTri, showTri) where

-- Convert a trinary string to a number.
-- Invalid input returns 0.
readTri :: Num a => String -> a
readTri = go 0
  where
    go acc [] = acc
    go acc (c:cs) =
      case charToDigit c of
        Nothing ->
          0
        Just d ->
          let acc' = acc * 3 + fromIntegral d
          in acc' `seq` go acc' cs

    charToDigit :: Char -> Maybe Int
    charToDigit '0' = Just 0
    charToDigit '1' = Just 1
    charToDigit '2' = Just 2
    charToDigit _   = Nothing

-- Convert a (non-negative) decimal number to its trinary string.
-- For 0 and negative inputs, return "0".
showTri :: Integral a => a -> String
showTri n
  | n <= 0    = "0"
  | otherwise = go n []
  where
    go 0 acc = acc
    go m acc =
      let (q, r) = quotRem m 3
          ch = case (fromIntegral r :: Int) of
                 0 -> '0'
                 1 -> '1'
                 _ -> '2'
          acc' = ch : acc  -- Prepend next most-significant digit
      in acc' `seq` go q acc'
