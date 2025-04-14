module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode rails msg = concat $ buildRails rails msg

decode :: Int -> String -> String
decode rails msg = readZigzag rails (length msg) (distribute rails msg)

-- Build the rails by placing characters in a zigzag pattern
buildRails :: Int -> String -> [String]
buildRails rails msg = map (map snd . filter ((/= '.') . snd)) $ splitIntoRails rails (zigzagPattern rails (length msg)) (msg ++ repeat '.')
  where
    repeat '.' = '.' : repeat '.'

-- Create a zigzag pattern for encoding
zigzag :: Int -> String -> [String]
zigzag rails msg = map (\r -> map snd $ filter (\(row, _) -> row == r) $ zip (zigzagPattern rails (length msg)) msg) [0..rails-1]

-- Generate the zigzag pattern as a list of rail numbers
zigzagPattern :: Int -> Int -> [Int]
zigzagPattern rails len = take len $ concat $ repeat $ down ++ tail (reverse down)
  where down = [0..rails-1]

-- Distribute the encoded message back into rails for decoding
distribute :: Int -> String -> [String]
distribute rails msg = splitIntoRails rails (zigzagPattern rails (length msg)) msg

-- Split the message into rails based on the zigzag pattern
splitIntoRails :: Int -> [Int] -> String -> [String]
splitIntoRails rails pattern msg = go rails pattern msg (replicate rails "")
  where
    go _ [] _ acc = acc
    go r (p:ps) (m:ms) acc = go r ps ms (replaceAt p (m :) acc)
    go _ _ _ acc = acc

-- Replace element at index with result of applying function
replaceAt :: Int -> (a -> a) -> [a] -> [a]
replaceAt n f xs = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

-- Read the message following the zigzag pattern for decoding
readZigzag :: Int -> Int -> [String] -> String
readZigzag rails len railsData = go (zigzagPattern rails len) railsData []
  where
    go [] _ acc = acc
    go (p:ps) rData acc = case rData !! p of
      (x:xs) -> go ps (replaceAt p (const xs) rData) (acc ++ [x])
      _ -> go ps rData acc
