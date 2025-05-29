module RailFenceCipher (encode, decode) where

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index f list = 
    let (a, x:bs) = splitAt index list
    in a ++ (f x : bs)

splitByCounts :: [Int] -> [a] -> [[a]]
splitByCounts [] _ = []
splitByCounts (c:cs) xs = 
    let (rail, rest) = splitAt c xs
    in rail : splitByCounts cs rest

indices :: Int -> Int -> [Int]
indices k n 
    | k <= 1    = replicate n 0
    | otherwise = map getIdx [0..n-1]
    where 
      period = 2*(k-1)
      getIdx i = 
          let r = i `mod` period
          in if r < k then r else period - r

encode :: Int -> String -> String
encode k s
    | k <= 1    = s
    | otherwise = 
        let n = length s
            idxs = indices k n
            rails = foldl (\rs (i, c) -> updateAt i (c:) rs) (replicate k []) (zip idxs s)
            in concat (map reverse rails)

decode :: Int -> String -> String
decode k s
    | k <= 1    = s
    | otherwise = 
        let n = length s
            idxs = indices k n
            counts = foldl (\cnts i -> updateAt i (+1) cnts) (replicate k 0) idxs
            rails = splitByCounts counts s
            (result, _) = foldl 
                (\(prev, rs) idx -> 
                    let (h:tail) = rs !! idx
                    in (h:prev, updateAt idx (const tail) rs)
                ) ([], rails) idxs
            in reverse result
