module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs 
    | n <= 0 = error "slice length must be positive"
    | n > length xs = []
    | otherwise = map (map (read . pure)) $ take (length xs - n + 1) $ map (take n) $ tails xs
    where
        tails [] = []
        tails ys@(_:rest) = ys : tails rest
