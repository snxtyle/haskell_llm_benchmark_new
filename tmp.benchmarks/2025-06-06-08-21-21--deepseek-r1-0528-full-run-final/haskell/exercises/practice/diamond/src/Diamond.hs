module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just [line i | i <- [0 .. totalLines - 1]]
  where
    n = fromEnum c - fromEnum 'A'
    totalLines = 2 * n + 1
    line i =
      let d = abs (i - n)
          ch = toEnum (fromEnum 'A' + n - d) :: Char
          outer = replicate d ' '
      in if d == n
          then outer ++ [ch] ++ outer
          else let inner = replicate (2 * (n - d) - 1) ' '
               in outer ++ [ch] ++ inner ++ [ch] ++ outer
