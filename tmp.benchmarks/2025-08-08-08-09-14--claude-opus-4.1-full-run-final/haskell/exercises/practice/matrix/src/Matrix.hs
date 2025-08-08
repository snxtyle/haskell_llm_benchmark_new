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
column colIdx matrix = 
    V.generate (matrixRows matrix) $ \rowIdx ->
        matrixData matrix V.! (rowIdx * matrixCols matrix + colIdx)

flatten :: Matrix a -> Vector a
flatten = matrixData

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss@(xs:_) = Matrix
    { matrixRows = length xss
    , matrixCols = length xs
    , matrixData = V.fromList (concat xss)
    }

fromString :: Read a => String -> Matrix a
fromString str = fromList $ map parseLine $ lines str
  where
    parseLine line = map read $ words line

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) matrix = Matrix
    { matrixRows = newRows
    , matrixCols = newCols
    , matrixData = matrixData matrix
    }

row :: Int -> Matrix a -> Vector a
row rowIdx matrix = 
    if matrixCols matrix == 0
    then V.empty
    else let startIdx = rowIdx * matrixCols matrix
         in V.slice startIdx (matrixCols matrix) (matrixData matrix)

rows :: Matrix a -> Int
rows = matrixRows

shape :: Matrix a -> (Int, Int)
shape matrix = (matrixRows matrix, matrixCols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix
    { matrixRows = matrixCols matrix
    , matrixCols = matrixRows matrix
    , matrixData = V.generate (matrixCols matrix * matrixRows matrix) $ \idx ->
        let newRow = idx `div` matrixRows matrix
            newCol = idx `mod` matrixRows matrix
            oldIdx = newCol * matrixCols matrix + newRow
        in matrixData matrix V.! oldIdx
    }
