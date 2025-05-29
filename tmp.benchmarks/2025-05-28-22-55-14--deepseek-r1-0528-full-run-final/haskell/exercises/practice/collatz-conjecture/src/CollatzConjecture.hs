module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just (loop n 0)
    where
        loop 1 count = count
        loop x count
            | even x    = loop (x `div` 2) (count+1)
            | otherwise = loop (3*x+1) (count+1)
