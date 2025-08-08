module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ makeDiamond c

makeDiamond :: Char -> [String]
makeDiamond c = topHalf ++ bottomHalf
  where
    n = fromEnum c - fromEnum 'A'
    size = 2 * n + 1
    topHalf = [makeLine i n | i <- [0..n]]
    bottomHalf = if n > 0 then [makeLine i n | i <- [n-1, n-2..0]] else []

makeLine :: Int -> Int -> String
makeLine rowIndex maxIndex = line
  where
    size = 2 * maxIndex + 1
    letter = toEnum (fromEnum 'A' + rowIndex)
    outerSpaces = maxIndex - rowIndex
    
    line = if rowIndex == 0
           then replicate outerSpaces ' ' ++ "A" ++ replicate outerSpaces ' '
           else 
             let innerSpaces = 2 * rowIndex - 1
                 leftPart = replicate outerSpaces ' ' ++ [letter]
                 middlePart = replicate innerSpaces ' '
                 rightPart = [letter] ++ replicate outerSpaces ' '
             in leftPart ++ middlePart ++ rightPart
