module Octal (readOct, showOct) where

readOct :: String -> Int
readOct s
  | all isOctalDigit s = helper s 0
  | otherwise = 0
  where
    isOctalDigit c = c >= '0' && c <= '7'
    helper [] acc = acc
    helper (x:xs) acc =
      let digit = fromEnum x - fromEnum '0'  -- Convert character to its digit value
          nextAcc = acc * 8 + fromIntegral digit  -- Accumulate the value
      in nextAcc `seq` helper xs nextAcc  -- Ensure strict evaluation

showOct :: Int -> String
showOct n
  | n <= 0 = "0"  -- Treat non-positive numbers as 0
  | otherwise = reverse $ helper n
  where
    helper k
      | k == 0 = []
      | otherwise =
          let remainder = k `mod` 8  -- Get the remainder
              remChar = toEnum (fromEnum '0' + fromIntegral remainder)  -- Convert remainder to character
          in remChar : helper (k `div` 8)  -- Recur and prepend the character
