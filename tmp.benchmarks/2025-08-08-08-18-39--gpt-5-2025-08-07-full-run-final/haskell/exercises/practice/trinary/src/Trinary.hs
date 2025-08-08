module Trinary (readTri, showTri) where

-- Convert a trinary string to its decimal value.
-- Returns 0 if the input contains any invalid character.
readTri :: Integral a => String -> a
readTri = go 0
  where
    go :: Integral a => a -> String -> a
    go acc [] = acc
    go acc (c:cs) =
      case c of
        '0' ->
          let acc' = acc * 3
          in acc' `seq` go acc' cs
        '1' ->
          let acc' = acc * 3 + 1
          in acc' `seq` go acc' cs
        '2' ->
          let acc' = acc * 3 + 2
          in acc' `seq` go acc' cs
        _ -> 0  -- invalid trinary string

-- Convert a non-negative Integral to its trinary string representation.
-- For n <= 0, returns "0".
showTri :: Integral a => a -> String
showTri n
  | n <= 0    = "0"
  | otherwise = toDigits n []
  where
    toDigits :: Integral a => a -> String -> String
    toDigits 0 acc = acc
    toDigits k acc =
      let (q, r) = k `quotRem` 3
          ch = toEnum (fromEnum '0' + fromIntegral r) :: Char
          acc' = ch : acc
      in acc' `seq` toDigits q acc'
