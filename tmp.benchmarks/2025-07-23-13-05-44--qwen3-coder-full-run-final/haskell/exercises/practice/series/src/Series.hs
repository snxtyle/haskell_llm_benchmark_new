module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = error "slice length must be positive"
  | n == 0 || n > length xs = []
  | otherwise = map (map charToInt) $ slidingWindow n xs
  where
    charToInt :: Char -> Int
    charToInt c = read [c]
    
    slidingWindow :: Int -> [a] -> [[a]]
    slidingWindow size list
      | size > length list = []
      | otherwise = take size list : slidingWindow size (drop 1 list)
