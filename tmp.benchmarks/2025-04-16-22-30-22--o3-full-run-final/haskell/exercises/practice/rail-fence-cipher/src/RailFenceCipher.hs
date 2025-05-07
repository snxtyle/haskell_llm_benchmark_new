module RailFenceCipher (encode, decode) where

-- | Produce the repeating zig‑zag pattern of rail indices.
--   For example, with 3 rails the finite pattern [0,1,2,1] is repeated:
--       0 1 2 1 0 1 2 1 ...
zigZagPattern :: Int -> [Int]
zigZagPattern n
  | n <= 1    = repeat 0
  | otherwise = cycle (up ++ down)
  where
    up   = [0 .. n - 1]
    down = [n - 2, n - 3 .. 1]

-- | Encode a message using a rail‑fence of the given height.
encode :: Int -> String -> String
encode rails text
  | rails <= 1 = text
  | otherwise  = concat [railChars r | r <- [0 .. rails - 1]]
  where
    pattern   = take (length text) (zigZagPattern rails)
    railChars r = [c | (c, p) <- zip text pattern, p == r]

-- | Split a list into chunks whose sizes are given by the input counts.
--   Example: splitByCounts [2,3] "abcdef" == ["ab","cde"]
splitByCounts :: [Int] -> [a] -> [[a]]
splitByCounts [] _ = []
splitByCounts (c:cs) xs =
  let (chunk, rest) = splitAt c xs
  in chunk : splitByCounts cs rest

-- | Pop (take head) from the list at the given index,
--   returning the element together with the updated list‑of‑lists.
popFrom :: Int -> [[a]] -> (a, [[a]])
popFrom idx lists =
  let (before, current : after) = splitAt idx lists
      (x : xs)                  = current
  in (x, before ++ (xs : after))

-- | Decode a rail‑fence cipher back into the original message.
decode :: Int -> String -> String
decode rails cipher
  | rails <= 1 = cipher
  | otherwise  = rebuild initialRails pattern
  where
    len      = length cipher
    pattern  = take len (zigZagPattern rails)

    -- How many characters belong to each rail?
    railCounts = [length (filter (== r) pattern) | r <- [0 .. rails - 1]]

    -- Split the cipher text into the rails row‑by‑row.
    initialRails :: [[Char]]
    initialRails = splitByCounts railCounts cipher

    -- Reconstruct the message by following the zig‑zag pattern
    rebuild :: [[Char]] -> [Int] -> String
    rebuild _      []       = []
    rebuild rails' (p : ps) =
      let (ch, rails'') = popFrom p rails'
      in ch : rebuild rails'' ps
