module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ buildDiamond c

buildDiamond :: Char -> [String]
buildDiamond c = topHalf ++ reverse (init topHalf)
  where
    n = fromEnum c - fromEnum 'A'
    topHalf = [buildRow i n | i <- [0..n]]

buildRow :: Int -> Int -> String
buildRow i n
  | i == 0 = replicate n ' ' ++ ['A'] ++ replicate n ' '
  | otherwise = 
      let letter = toEnum (fromEnum 'A' + i)
          leadingSpaces = replicate (n - i) ' '
          innerSpaces = replicate (2 * i - 1) ' '
      in leadingSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ leadingSpaces
