module RailFenceCipher (encode, decode) where

-- | Produce the rail‑fence cipher encoding of a given plaintext.
--   For one rail (or fewer) the text is returned unchanged.
encode :: Int -> String -> String
encode rails text
  | rails <= 1 = text
  | otherwise  = concatMap charsOnRail [0 .. rails - 1]
  where
    indices        = take (length text) (railPattern rails)
    zipped         = zip indices text
    charsOnRail r  = [c | (idx, c) <- zipped, idx == r]

-- | Decode a text that was encoded with the rail‑fence cipher.
--   For one rail (or fewer) the text is returned unchanged.
decode :: Int -> String -> String
decode rails cipher
  | rails <= 1 = cipher
  | otherwise  = rebuild pattern initialRows
  where
    len       = length cipher
    pattern   = take len (railPattern rails)

    -- How many characters belong to each rail?
    counts    = [length (filter (== r) pattern) | r <- [0 .. rails - 1]]

    -- Split the ciphertext into the rows, in the same order they were written
    initialRows = splitRows counts cipher

    -- Re‑assemble the original text by walking along the zig‑zag pattern
    rebuild :: [Int] -> [String] -> String
    rebuild [] _ = []
    rebuild (r:rs) rows =
      case rows !! r of
        []       -> error "RailFenceCipher.decode: encountered empty rail"
        (c:rest) ->
          let rows' = replaceAt r rest rows
          in  c : rebuild rs rows'

    -- Split the cipher text into a list of rows having the given lengths.
    splitRows :: [Int] -> String -> [String]
    splitRows [] _  = []
    splitRows (n:ns) s =
      let (rowPart, rest) = splitAt n s
      in  rowPart : splitRows ns rest

    -- Replace the element at position n in a list.
    replaceAt :: Int -> a -> [a] -> [a]
    replaceAt n newVal xs =
      let (before, _ : after) = splitAt n xs
      in  before ++ newVal : after

-- | Infinite list describing the rail pattern (0‑based rail numbers).
--   Example for 3 rails: 0,1,2,1,0,1,2,1,0,…
railPattern :: Int -> [Int]
railPattern n
  | n <= 1    = repeat 0
  | otherwise = cycle (up ++ down)
  where
    up   = [0 .. n - 1]
    down = [n - 2, n - 3 .. 1]
