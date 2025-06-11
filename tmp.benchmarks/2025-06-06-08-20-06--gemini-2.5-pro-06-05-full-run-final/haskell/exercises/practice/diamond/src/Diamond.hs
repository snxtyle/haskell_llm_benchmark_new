module Diamond (diamond) where

import Data.Char (isUpper, ord)

diamond :: Char -> Maybe [String]
diamond c
  | not (isUpper c) = Nothing
  | otherwise       = Just (topHalf ++ bottomHalf)
  where
    n = ord c - ord 'A'
    chars = ['A'..c]

    mkLine :: Char -> String
    mkLine l = outerPadding ++ content ++ outerPadding
      where
        i = ord l - ord 'A'
        outerPadding = replicate (n - i) ' '
        content
          | l == 'A'  = "A"
          | otherwise = [l] ++ replicate (2 * i - 1) ' ' ++ [l]

    topHalf = map mkLine chars
    bottomHalf = reverse (init topHalf)
