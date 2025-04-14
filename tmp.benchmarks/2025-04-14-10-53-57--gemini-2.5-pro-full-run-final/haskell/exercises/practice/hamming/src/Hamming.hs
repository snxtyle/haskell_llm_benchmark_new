module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just (calculateDistance xs ys)
    where
        calculateDistance :: String -> String -> Int
        calculateDistance [] [] = 0
        calculateDistance (x:xt) (y:yt)
            | x /= y    = 1 + calculateDistance xt yt
            | otherwise = calculateDistance xt yt
        calculateDistance _ _ = error "This should not happen due to length check" -- Added for completeness, though the guard prevents this case
