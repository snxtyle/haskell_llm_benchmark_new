module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ createDiamond c

createDiamond :: Char -> [String]
createDiamond c = topHalf ++ bottomHalf
  where
    size = ord c - ord 'A'
    topHalf = [createRow i size | i <- [0..size]]
    bottomHalf = if size > 0 then reverse (init topHalf) else []
    
createRow :: Int -> Int -> String
createRow rowIndex maxIndex
  | rowIndex == 0 = outerSpaces ++ "A" ++ outerSpaces
  | otherwise = outerSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ outerSpaces
  where
    letter = chr (ord 'A' + rowIndex)
    outerSpaces = replicate (maxIndex - rowIndex) ' '
    innerSpaces = replicate (2 * rowIndex - 1) ' '

-- Helper functions for character manipulation
ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum
