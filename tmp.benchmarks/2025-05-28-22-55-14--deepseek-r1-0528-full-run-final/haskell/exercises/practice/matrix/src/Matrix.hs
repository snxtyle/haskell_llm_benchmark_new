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

import qualified Data.Vector as V
import Data.Vector (Vector)

data Matrix a = Matrix !Int !Int !(Vector a) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix _ c _) = c

column :: Int -> Matrix a -> Vector a
column j (Matrix r c vec) = V.generate r 
  (\i -> vec V.! (i * c + (j - 1)))

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ vec) = vec

fromList :: [[a]] -> Matrix a
fromList xss = Matrix r c vec
  where
    r = length xss
    c = if r > 0 then length (head xss) else 0
    vec = V.fromList $ concat xss

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read . words) $ lines xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r', c') (Matrix _ _ vec) = Matrix r' c' vec

row :: Int -> Matrix a -> Vector a
row i (Matrix _ c vec) = V.slice ((i - 1) * c) c vec

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

transpose :: Matrix a -> Matrix a
transpose (Matrix r c vec) = Matrix c r $ 
  V.generate (r * c) (\k -> 
    let (i, j) = k `divMod` r 
    in vec V.! (j * c + i)
  )
