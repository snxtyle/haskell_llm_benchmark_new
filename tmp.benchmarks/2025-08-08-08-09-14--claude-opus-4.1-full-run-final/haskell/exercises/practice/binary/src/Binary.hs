module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs 
  | not (isValidBinary xs) = 0  -- Return 0 for invalid input instead of error
  | otherwise = binaryToDecimal xs

isValidBinary :: String -> Bool
isValidBinary [] = True  -- Empty string should be valid (represents 0)
isValidBinary xs = all (\c -> c == '0' || c == '1') xs

binaryToDecimal :: String -> Int
binaryToDecimal xs = sum $ zipWith (*) powers digits
  where
    -- Reverse the string to process from right to left
    reversed = reverse xs
    -- Convert each character to its numeric value
    digits = map (\c -> if c == '1' then 1 else 0) reversed
    -- Generate powers of 2: [2^0, 2^1, 2^2, ...]
    powers = map (2^) [(0::Int)..]  -- Explicit type annotation to avoid warning
