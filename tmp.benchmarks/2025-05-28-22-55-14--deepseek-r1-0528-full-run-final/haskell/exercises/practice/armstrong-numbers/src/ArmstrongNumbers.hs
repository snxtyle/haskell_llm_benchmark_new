module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n
    | n < 0 = False
    | otherwise = 
        let digitList = if n == 0 then [0] else go n
            len = length digitList
        in n == sum (map (^ len) digitList)
    where
        go 0 = []
        go x = let (q, r) = x `divMod` 10 in r : go q
