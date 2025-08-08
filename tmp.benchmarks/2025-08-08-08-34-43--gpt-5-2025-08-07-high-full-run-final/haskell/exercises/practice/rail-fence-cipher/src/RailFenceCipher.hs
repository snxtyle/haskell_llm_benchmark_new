module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode rails xs
  | rails <= 1 = xs
  | null xs    = xs
  | otherwise  =
      let idx = take (length xs) (zigzag rails)
      in concat [ [ c | (c, i) <- zip xs idx, i == row ] | row <- [0 .. rails - 1] ]

decode :: Int -> String -> String
decode rails xs
  | rails <= 1 = xs
  | null xs    = xs
  | otherwise  =
      let n        = length xs
          idx      = take n (zigzag rails)
          counts   = [ count row idx | row <- [0 .. rails - 1] ]
          segments = splitByCounts counts xs
      in rebuild idx segments

-- Produce an infinite zig-zag sequence of rail indices: 0..r-1..1..0..r-1...
zigzag :: Int -> [Int]
zigzag r
  | r <= 1    = repeat 0
  | r == 2    = cycle [0,1]
  | otherwise = cycle ([0 .. r - 1] ++ [r - 2, r - 3 .. 1])

-- Count occurrences of a in list
count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

-- Split a list into chunks according to a list of sizes
splitByCounts :: [Int] -> [a] -> [[a]]
splitByCounts [] _ = []
splitByCounts (c:cs) xs =
  let (h, t) = splitAt c xs
  in h : splitByCounts (cs) t

-- Rebuild the original sequence by consuming from the appropriate rail segment
rebuild :: [Int] -> [[a]] -> [a]
rebuild [] _ = []
rebuild (k:ks) rails =
  let (c, rails') = popFrom k rails
  in c : rebuild ks rails'

-- Pop the head element from the k-th list and return it along with the updated lists
popFrom :: Int -> [[a]] -> (a, [[a]])
popFrom k rails =
  let (prefix, rest) = splitAt k rails
  in case rest of
       [] -> error "popFrom: index out of bounds"
       (bucket:suffix) ->
         case bucket of
           []       -> error "popFrom: empty bucket"
           (c:cs) -> (c, prefix ++ (cs : suffix))
