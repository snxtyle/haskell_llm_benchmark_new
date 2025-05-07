module Diamond (diamond) where

import Data.Char (isUpper, ord)

-- Generates a single row of the diamond.
-- maxChar: The character at the widest point of the diamond.
-- currentChar: The character for the current row.
mkRow :: Char -> Char -> String
mkRow maxChar currentChar =
  let maxIdx = ord maxChar - ord 'A'
      currentIdx = ord currentChar - ord 'A'
      
      outerSpacesCount = maxIdx - currentIdx
      outerPadding = replicate outerSpacesCount ' '
      
  in if currentChar == 'A'
     then outerPadding ++ "A" ++ outerPadding
     else -- For characters other than 'A', there are two letters and inner spaces
          let innerSpacesCount = 2 * currentIdx - 1
              innerPadding = replicate innerSpacesCount ' '
          in outerPadding ++ [currentChar] ++ innerPadding ++ [currentChar] ++ outerPadding

-- Generates the complete list of strings for the diamond.
generateDiamond :: Char -> [String]
generateDiamond maxChar =
  let maxIdx = ord maxChar - ord 'A'
      -- Characters from 'A' up to the maxChar for the top half
      charsForTopHalf = map (\i -> toEnum (ord 'A' + i)) [0..maxIdx]
      
      topHalf = map (mkRow maxChar) charsForTopHalf
      
      -- Bottom half is the reverse of the top half, excluding the middle row (which is already in topHalf)
      bottomHalf = reverse (init topHalf) 
      
  in topHalf ++ bottomHalf

-- The main function to create the diamond.
-- Takes a character and returns Maybe [String].
-- Returns Nothing if the character is not an uppercase letter.
diamond :: Char -> Maybe [String]
diamond c
  | not (isUpper c) = Nothing
  | otherwise       = Just (generateDiamond c)
