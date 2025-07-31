module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose rows = 
    let maxWidth = if null rows then 0 else maximum (map length rows)
        paddedRows = [replicate (maxWidth - length row) ' ' ++ row | row <- rows]
    in if maxWidth == 0 
       then []
       else [removeTrailingSpaces (map (!! col) paddedRows) | col <- [0..maxWidth-1]]
    where
        removeTrailingSpaces str = 
            if all (== ' ') str
            then str  -- If all spaces, keep them
            else reverse (dropWhile (== ' ') (reverse str))
