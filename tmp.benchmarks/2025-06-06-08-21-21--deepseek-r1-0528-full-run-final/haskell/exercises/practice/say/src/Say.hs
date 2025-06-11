module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = 
      let chunks = chunkify n
          scales = ["", "thousand", "million", "billion"]
          indexedChunks = zip [0..] chunks
          convertChunk (index, chunk) = 
            case convertUnder1000 chunk of
              Nothing -> Nothing
              Just s -> 
                if index == 0
                   then Just s
                   else Just (s ++ " " ++ scales !! index)
          pieces = reverse [ s | Just s <- map convertChunk indexedChunks ]
      in Just (unwords pieces)

chunkify :: Integer -> [Integer]
chunkify 0 = []
chunkify n = 
    let (q, r) = n `divMod` 1000
    in r : chunkify q

convertUnder1000 :: Integer -> Maybe String
convertUnder1000 0 = Nothing
convertUnder1000 n 
  | n < 20 = Just (small !! fromIntegral n)
  | n < 100 = 
      let (tensDigit, onesDigit) = n `divMod` 10
          tens = tensArray !! (fromIntegral tensDigit - 2)
      in if onesDigit == 0
          then Just tens
          else Just (tens ++ "-" ++ (small !! fromIntegral onesDigit))
  | n < 1000 = 
      let (hundredsDigit, rest) = n `divMod` 100
      in case convertUnder1000 rest of
          Nothing -> Just (small !! fromIntegral hundredsDigit ++ " hundred")
          Just s  -> Just (small !! fromIntegral hundredsDigit ++ " hundred " ++ s)

small :: [String]
small = [ "zero", "one", "two", "three", "four", "five", "six", "seven",
          "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
          "sixteen", "seventeen", "eighteen", "nineteen" ]

tensArray :: [String]
tensArray = [ "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ]
