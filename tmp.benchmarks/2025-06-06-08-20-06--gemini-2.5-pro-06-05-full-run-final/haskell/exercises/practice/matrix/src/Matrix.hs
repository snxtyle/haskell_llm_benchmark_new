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
column j (Matrix r c vec) = V.generate r (\i -> vec V.! (i * c + j - 1))

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ vec) = vec

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss@(xs:_) = Matrix (length xss) (length xs) (V.fromList (concat xss))

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix _ _ vec) = Matrix r c vec

row :: Int -> Matrix a -> Vector a
row r (Matrix _ c vec) = V.slice ((r - 1) * c) c vec

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

transpose :: Matrix a -> Matrix a
transpose m@(Matrix r c _) = Matrix c r (V.concat $ map (`column` m) [1..c])
