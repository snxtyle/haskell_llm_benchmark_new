module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ buildDiamond c

buildDiamond :: Char -> [String]
buildDiamond 'A' = ["A"]
buildDiamond c = topHalf ++ (reverse $ init topHalf)
  where
    topHalf = [makeRow letter (fromEnum c - fromEnum 'A') | letter <- ['A'..c]]
    
makeRow :: Char -> Int -> String
makeRow letter maxIndex
  | letter == 'A' = spaces ++ [letter] ++ spaces
  | otherwise = spaces ++ [letter] ++ middleSpaces ++ [letter] ++ spaces
  where
    currentIndex = fromEnum letter - fromEnum 'A'
    spaces = replicate (maxIndex - currentIndex) ' '
    middleSpaces = replicate (2 * currentIndex - 1) ' '
