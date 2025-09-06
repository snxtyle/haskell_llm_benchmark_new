module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
    | n <= 0 = []
    | otherwise = take n pascalsTriangle

-- Generate the infinite Pascal's triangle
pascalsTriangle :: [[Integer]]
pascalsTriangle = iterate nextRow [1]
  where
    nextRow row = [1] ++ zipWith (+) row (tail row) ++ [1]
