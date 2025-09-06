module RailFenceCipher (encode, decode) where

splitInto :: [Int] -> [a] -> [[a]]
splitInto [] _ = []
splitInto (l:ls) xs = let (a, b) = splitAt l xs in a : splitInto ls b

getRailIndices :: Int -> Int -> [Int]
getRailIndices n len = take len $ cycle pattern
  where pattern = [0 .. n-1] ++ [n-2, n-3 .. 1]

encode :: Int -> String -> String
encode n str
  | n <= 1 = str
  | otherwise = concat rails
  where indices = getRailIndices n (length str)
        indexed = zip indices str
        rails = [ [c | (i, c) <- indexed, i == r] | r <- [0..n-1] ]

decode :: Int -> String -> String
decode n cipher
  | n <= 1 = cipher
  | otherwise = fst $ foldl step ([], replicate n 0) indices
  where indices = getRailIndices n (length cipher)
        lengths = [length $ filter (== r) indices | r <- [0..n-1]]
        segments = splitInto lengths cipher
        step (res, cnts) r = let c = segments !! r !! (cnts !! r)
                                 newCnts = update cnts r (+1)
                             in (res ++ [c], newCnts)
        update xs i f = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs
