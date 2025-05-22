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

data Matrix a = Matrix Int Int (Vector a) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix _ c _) = c

column :: Int -> Matrix a -> Vector a
column colIdx (Matrix r c vec) = 
    V.generate r (\rowIdx -> vec V.! (rowIdx * c + colIdx))

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ vec) = vec

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss@(xs:_) = 
    let r = length xss
        c = length xs
        vec = V.fromList (concat xss)
    in Matrix r c vec

fromString :: Read a => String -> Matrix a
fromString str = 
    let lns = lines str
        parsedLines = map (map read . words) lns
    in fromList parsedLines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) (Matrix _ _ vec) = Matrix newRows newCols vec

row :: Int -> Matrix a -> Vector a
row rowIdx (Matrix _ c vec) = 
    V.slice (rowIdx * c) c vec

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

transpose :: Matrix a -> Matrix a
transpose matrix@(Matrix r c _) = 
    let newVec = V.generate (r * c) (\i -> 
            let newRow = i `div` r
                newCol = i `mod` r
            in (flatten matrix) V.! (newCol * c + newRow))
    in Matrix c r newVec
