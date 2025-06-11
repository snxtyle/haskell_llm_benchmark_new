module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate [] = []
annotate board@(row:_)
    | null row  = board
    | otherwise = zipWith (zipWith processChar) coords board
    where
        height = length board
        width = length row
        coords = [[(r, c) | c <- [0..width-1]] | r <- [0..height-1]]

        processChar :: (Int, Int) -> Char -> Char
        processChar (r, c) char
            | char == '*' = '*'
            | otherwise   = countToChar (countMines r c)

        countMines :: Int -> Int -> Int
        countMines r c = length [() | (dr, dc) <- offsets,
                                      let nr = r + dr,
                                      let nc = c + dc,
                                      inBounds nr nc,
                                      isMine nr nc]

        inBounds :: Int -> Int -> Bool
        inBounds r c = r >= 0 && r < height && c >= 0 && c < width

        isMine :: Int -> Int -> Bool
        isMine r c = (board !! r !! c) == '*'

        offsets :: [(Int, Int)]
        offsets = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

        countToChar :: Int -> Char
        countToChar 0 = ' '
        countToChar n = intToDigit n
