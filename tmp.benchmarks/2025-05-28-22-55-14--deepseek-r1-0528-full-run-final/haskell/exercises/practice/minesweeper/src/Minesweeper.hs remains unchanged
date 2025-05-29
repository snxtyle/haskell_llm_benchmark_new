module Minesweeper (annotate) where

import Data.Char (intToDigit)
import qualified Data.Set as S

annotate :: [String] -> [String]
annotate board = 
    let rows = length board
        cols = if rows == 0 then 0 else length (head board)
        mineSet = S.fromList [ (i, j) | (i, row) <- zip [0..] board, 
                                        (j, cell) <- zip [0..] row, 
                                        cell == '*' ]
        charAt i j 
            | (i,j) `S.member` mineSet = '*'
            | otherwise = 
                let cnt = length [ () | di <- [-1,0,1], dj <- [-1,0,1], 
                                        (di,dj) /= (0,0),
                                        S.member (i+di, j+dj) mineSet ]
                in if cnt == 0 then ' ' else intToDigit cnt
    in [ [ charAt i j | j <- [0..cols-1] ] | i <- [0..rows-1] ]
