module Diamond (diamond) where

import Data.Char (ord)

diamond :: Char -> Maybe [String]
diamond c
  | not (isUpperAtoZ c) = Nothing
  | otherwise = Just (rows ++ reverse (init rows))
  where
    rows = map (row c) ['A' .. c]

isUpperAtoZ :: Char -> Bool
isUpperAtoZ ch = 'A' <= ch && ch <= 'Z'

row :: Char -> Char -> String
row maxCh ch =
  let n       = ord maxCh - ord 'A'
      i       = ord ch - ord 'A'
      leading = n - i
      inner   = if i == 0 then 0 else 2 * i - 1
      left    = replicate leading ' '
      mid     = replicate inner ' '
  in if ch == 'A'
       then left ++ "A" ++ left
       else left ++ [ch] ++ mid ++ [ch] ++ left
