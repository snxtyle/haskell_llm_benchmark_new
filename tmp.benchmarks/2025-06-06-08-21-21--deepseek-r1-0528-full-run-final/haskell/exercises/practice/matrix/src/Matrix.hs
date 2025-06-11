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

newtype Matrix a = Matrix (V.Vector (Vector a))
    deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m) = 
    if V.null m 
        then 0 
        else V.length (V.head m)

column :: Int -> Matrix a -> Vector a
column j (Matrix m) = V.map (V.! j) m

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat (V.toList m)

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

fromString :: Read a => String -> Matrix a
fromString s = fromList (map (map read . words) (lines s))

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m = 
    let flat = flatten m
    in Matrix $ V.generate r $ \i -> 
            V.generate c $ \j -> 
                flat V.! (i * c + j)

row :: Int -> Matrix a -> Vector a
row i (Matrix m) = m V.! i

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose (Matrix m) = 
    if V.null m 
        then Matrix V.empty
        else 
            let numCols = V.length (V.head m)
                numRows = V.length m
                newRows = V.generate numCols $ \j ->
                            V.generate numRows $ \i -> 
                                m V.! i V.! j
            in Matrix newRows
