module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n < 1     = Nothing
    | otherwise = 
        let s = aliquotSum n
        in Just $ if s == n then Perfect
                  else if s > n then Abundant
                  else Deficient
    where
        aliquotSum n
            | n == 1 = 0
            | otherwise = 
                let sqrtN = floor (sqrt (fromIntegral n))
                    divisors = [ d | d <- [1..sqrtN], n `mod` d == 0 ]
                    divisors' = concatMap (\d -> 
                                if d*d == n 
                                   then [d]
                                   else [d, n `div` d]
                        ) divisors
                    divisors'' = filter (/= n) divisors'
                in sum divisors''
