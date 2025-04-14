module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (convert n)

convert :: Integer -> String
convert n = 
  let (chunks, _) = breakIntoChunks n 1000 []
      namedChunks = zip chunks (reverse ("" : scales))
      nonZeroChunks = filter (\(num, _) -> num > 0) namedChunks
      formatted = map formatChunk nonZeroChunks
  in unwords formatted
  where
    scales = ["thousand", "million", "billion", "trillion"]

breakIntoChunks :: Integer -> Integer -> [Integer] -> ([Integer], Integer)
breakIntoChunks 0 _ chunks = (chunks, 0)
breakIntoChunks n divisor chunks =
  let (quotient, remainder) = n `divMod` divisor
  in breakIntoChunks quotient divisor (remainder : chunks)

formatChunk :: (Integer, String) -> String
formatChunk (n, scale)
  | n < 1000 = underThousand n ++ addScale scale
  | otherwise = convert n ++ addScale scale
  where
    addScale "" = ""
    addScale s = " " ++ s

underThousand :: Integer -> String
underThousand n
  | n < 20 = smallNumbers !! fromIntegral n
  | n < 100 = let (tens, ones) = n `divMod` 10
                  tensWord = tensWords !! fromIntegral tens
                  onesWord = if ones > 0 then "-" ++ (smallNumbers !! fromIntegral ones) else ""
              in tensWord ++ onesWord
  | otherwise = let (hundreds, rest) = n `divMod` 100
                    hundredsWord = (smallNumbers !! fromIntegral hundreds) ++ " hundred"
                    restWord = if rest > 0 then " " ++ underThousand rest else ""
                in hundredsWord ++ restWord
  where
    smallNumbers = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                    "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
                    "seventeen", "eighteen", "nineteen"]
    tensWords = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
