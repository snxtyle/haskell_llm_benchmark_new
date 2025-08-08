module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ unwords $ filter (not . null) $ reverse $ buildWords $ reverse $ chunks n
  where
    -- Break number into chunks of 1000s
    chunks :: Integer -> [Integer]
    chunks 0 = []
    chunks x = (x `mod` 1000) : chunks (x `div` 1000)
    
    -- Build words from chunks with scale words
    buildWords :: [Integer] -> [String]
    buildWords cs = zipWith addScale cs scales
    
    -- Scale words for each position
    scales :: [String]
    scales = ["", "thousand", "million", "billion", "trillion"]
    
    -- Add scale word to chunk if non-zero
    addScale :: Integer -> String -> String
    addScale 0 _ = ""
    addScale chunk scale = 
      let chunkWords = sayHundreds chunk
      in if null scale 
         then chunkWords
         else if null chunkWords
              then ""
              else chunkWords ++ " " ++ scale
    
    -- Convert 0-999 to words
    sayHundreds :: Integer -> String
    sayHundreds num
      | num == 0 = ""
      | num < 100 = sayTens num
      | otherwise = 
          let hundreds = num `div` 100
              remainder = num `mod` 100
          in if remainder == 0
             then ones !! fromIntegral hundreds ++ " hundred"
             else ones !! fromIntegral hundreds ++ " hundred " ++ sayTens remainder
    
    -- Convert 0-99 to words
    sayTens :: Integer -> String
    sayTens num
      | num == 0 = ""
      | num < 20 = ones !! fromIntegral num
      | otherwise = 
          let tensDigit = num `div` 10
              onesDigit = num `mod` 10
          in if onesDigit == 0
             then tens !! fromIntegral tensDigit
             else tens !! fromIntegral tensDigit ++ "-" ++ ones !! fromIntegral onesDigit
    
    -- Words for 0-19
    ones :: [String]
    ones = ["", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
            "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    
    -- Words for tens (20, 30, 40, ...)
    tens :: [String]
    tens = ["", "", "twenty", "thirty", "forty", "fifty", 
            "sixty", "seventy", "eighty", "ninety"]
