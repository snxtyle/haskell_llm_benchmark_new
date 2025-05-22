module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ unwords $ filter (not . null) $ reverse $ addScales $ reverse $ chunksOf3 n

-- Break number into chunks of 3 digits
chunksOf3 :: Integer -> [Integer]
chunksOf3 0 = []
chunksOf3 n = (n `mod` 1000) : chunksOf3 (n `div` 1000)

-- Add scale words to chunks
addScales :: [Integer] -> [String]
addScales chunks = zipWith addScale chunks scales
  where
    scales = ["", "thousand", "million", "billion", "trillion"]
    addScale 0 _ = ""
    addScale chunk "" = sayHundreds chunk
    addScale chunk scale = sayHundreds chunk ++ " " ++ scale

-- Convert a number 0-999 to words
sayHundreds :: Integer -> String
sayHundreds n
  | n == 0 = ""
  | n < 100 = sayTens n
  | otherwise = 
      let hundreds = n `div` 100
          remainder = n `mod` 100
      in ones !! fromIntegral hundreds ++ " hundred" ++ 
         if remainder == 0 then "" else " " ++ sayTens remainder

-- Convert a number 0-99 to words
sayTens :: Integer -> String
sayTens n
  | n < 20 = ones !! fromIntegral n
  | otherwise = 
      let tensDigit = n `div` 10
          onesDigit = n `mod` 10
      in tens !! fromIntegral tensDigit ++ 
         if onesDigit == 0 then "" else "-" ++ ones !! fromIntegral onesDigit

-- Word lists
ones :: [String]
ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
        "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
