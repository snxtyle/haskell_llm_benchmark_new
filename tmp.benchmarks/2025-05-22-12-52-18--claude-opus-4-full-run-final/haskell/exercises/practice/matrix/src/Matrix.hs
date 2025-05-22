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

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m) 
    | V.null m = 0
    | otherwise = V.length (V.head m)

column :: Int -> Matrix a -> Vector a
column x (Matrix m) = V.map (V.! (x - 1)) m

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat (V.toList m)

fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (map V.fromList xss))

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read . words) (lines xs)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix = 
    let flat = flatten matrix
        chunks = takeChunks c (V.toList flat)
    in Matrix (V.fromList (map V.fromList (take r chunks)))
  where
    takeChunks _ [] = []
    takeChunks n xs = take n xs : takeChunks n (drop n xs)

row :: Int -> Matrix a -> Vector a
row x (Matrix m) = m V.! (x - 1)

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose (Matrix m)
    | V.null m = Matrix V.empty
    | otherwise = Matrix $ V.fromList 
        [ V.fromList [ (m V.! r) V.! c | r <- [0..rows (Matrix m) - 1] ]
        | c <- [0..cols (Matrix m) - 1]
        ]
