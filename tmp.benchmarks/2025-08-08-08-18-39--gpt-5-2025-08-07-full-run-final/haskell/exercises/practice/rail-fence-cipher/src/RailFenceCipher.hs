module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode rails xs
  | rails <= 1 = xs
  | null xs = xs
  | otherwise =
      concat [[c | (c, i) <- zipped, i == r] | r <- [0 .. rails - 1]]
  where
    zigzag = [0 .. rails - 1] ++ [rails - 2, rails - 3 .. 1]
    idxs = take (length xs) (cycle zigzag)
    zipped = zip xs idxs

decode :: Int -> String -> String
decode rails cs
  | rails <= 1 = cs
  | null cs = cs
  | otherwise = go idxs segments []
  where
    n = length cs
    zigzag = [0 .. rails - 1] ++ [rails - 2, rails - 3 .. 1]
    idxs = take n (cycle zigzag)
    counts = [length (filter (== r) idxs) | r <- [0 .. rails - 1]]
    segments = cut counts cs

    cut :: [Int] -> [a] -> [[a]]
    cut [] _ = []
    cut (k:ks) s =
      let (h, t) = splitAt k s
       in h : cut ks t

    takeFrom :: Int -> [[a]] -> (a, [[a]])
    takeFrom i lists =
      case splitAt i lists of
        (before, listI:after) ->
          case listI of
            (x:xs) -> (x, before ++ (xs : after))
            [] -> error "Unexpected empty rail during decoding"
        _ -> error "Invalid rail index during decoding"

    go :: [Int] -> [[a]] -> [a] -> [a]
    go [] _ acc = reverse acc
    go (i:is) lists acc =
      let (x, lists') = takeFrom i lists
       in go is lists' (x : acc)
