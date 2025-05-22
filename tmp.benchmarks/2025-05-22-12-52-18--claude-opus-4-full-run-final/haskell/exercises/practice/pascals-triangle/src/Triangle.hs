module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x
  | x <= 0    = []
  | otherwise = take x $ iterate nextRow [1]
  where
    nextRow :: [Integer] -> [Integer]
    nextRow row = 1 : zipWith (+) row (tail row) ++ [1]
