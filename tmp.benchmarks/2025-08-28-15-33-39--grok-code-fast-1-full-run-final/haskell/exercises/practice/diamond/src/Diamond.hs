module Diamond (diamond) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (ord, chr, isUpper)

diamond :: Char -> Maybe [Text]
diamond c
  | not (isUpper c) = Nothing
  | otherwise = Just $ map (T.pack . makeRow) [0 .. 2*size]
  where
    size = ord c - ord 'A'
    width = 2*size + 1
    center = size
    makeRow i = 
      let letter = chr (ord 'A' + min i (2*size - i))
          dist = ord letter - ord 'A'
          positions = if dist == 0 then [center] else [center - dist, center + dist]
          row = replicate width ' '
      in foldl (\acc pos -> take pos acc ++ [letter] ++ drop (pos+1) acc) row positions
