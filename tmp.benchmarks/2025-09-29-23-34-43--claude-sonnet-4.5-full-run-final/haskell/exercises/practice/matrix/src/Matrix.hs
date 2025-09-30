module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Matrix a = Matrix 
    { matrixRows :: Int
    , matrixCols :: Int
    , matrixData :: Vector a
    } deriving (Eq, Show)

cols :: Matrix a -> Int
cols = matrixCols

column :: Int -> Matrix a -> Vector a
column colIdx matrix = V.generate (matrixRows matrix) $ \rowIdx ->
    matrixData matrix V.! (rowIdx * matrixCols matrix + colIdx)

flatten :: Matrix a -> Vector a
flatten = matrixData

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss@(firstRow:_) = 
    let numRows = length xss
        numCols = length firstRow
        flatData = V.fromList (concat xss)
    in Matrix numRows numCols flatData

fromString :: Read a => String -> Matrix a
fromString xs = 
    let linesOfText = lines xs
        parsedRows = map (map read . words) linesOfText
    in fromList parsedRows

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) matrix = 
    Matrix newRows newCols (matrixData matrix)

row :: Int -> Matrix a -> Vector a
row rowIdx matrix = 
    let start = rowIdx * matrixCols matrix
    in V.slice start (matrixCols matrix) (matrixData matrix)

rows :: Matrix a -> Int
rows = matrixRows

shape :: Matrix a -> (Int, Int)
shape matrix = (matrixRows matrix, matrixCols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = 
    let numRows = matrixRows matrix
        numCols = matrixCols matrix
        newData = V.generate (numRows * numCols) $ \i ->
            let newRow = i `div` numRows
                newCol = i `mod` numRows
                oldIdx = newCol * numCols + newRow
            in matrixData matrix V.! oldIdx
    in Matrix numCols numRows newData
