module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal = go 0
  where
    go :: Int -> String -> Int
    go acc [] = acc
    go acc (c:cs) =
      case c of
        '0' -> go (acc * 2) cs
        '1' -> go (acc * 2 + 1) cs
        _   -> 0
