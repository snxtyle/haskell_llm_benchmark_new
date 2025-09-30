module Diamond (diamond) where

import Data.Char (ord, chr, isUpper)

diamond :: Char -> Maybe [String]
diamond c
  | not (isUpper c) || c < 'A' || c > 'Z' = Nothing
  | otherwise = Just (makeDiamond c)

makeDiamond :: Char -> [String]
makeDiamond c = topHalf ++ bottomHalf
  where
    n = ord c - ord 'A'
    letters = ['A' .. c]
    topHalf = map (makeLine n) [0..n]
    bottomHalf = reverse (init topHalf)

makeLine :: Int -> Int -> String
makeLine n i = leadingSpaces ++ leftLetter ++ middleSpaces ++ rightLetter ++ trailingSpaces
  where
    letter = chr (ord 'A' + i)
    outerSpaces = n - i
    innerSpaces = 2 * i - 1
    
    leadingSpaces = replicate outerSpaces ' '
    trailingSpaces = replicate outerSpaces ' '
    leftLetter = [letter]
    
    -- For 'A' (i=0), there's no right letter or middle spaces
    (middleSpaces, rightLetter) = if i == 0
                                   then ("", "")
                                   else (replicate innerSpaces ' ', [letter])
