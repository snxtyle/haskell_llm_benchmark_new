module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = mapWithIndex (processRow board) board

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = zipWith f [0..] xs

processRow :: [String] -> Int -> String -> String
processRow board rowIdx row = mapWithIndex (processCell board rowIdx) row

processCell :: [String] -> Int -> Int -> Char -> Char
processCell board rowIdx colIdx cell
    | cell == '*' = '*'
    | otherwise = countMines board rowIdx colIdx

countMines :: [String] -> Int -> Int -> Char
countMines board rowIdx colIdx =
    let height = length board
        width = if height > 0 then length (head board) else 0
        adjacentCells = [(r, c) | r <- [rowIdx-1..rowIdx+1], 
                                c <- [colIdx-1..colIdx+1], 
                                r >= 0 && r < height, 
                                c >= 0 && c < width,
                                (r, c) /= (rowIdx, colIdx)]
        mineCount = length $ filter (\(r, c) -> (board !! r) !! c == '*') adjacentCells
    in if mineCount > 0 then head (show mineCount) else ' '
