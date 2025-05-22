module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (sayNumber n)

sayNumber :: Integer -> String
sayNumber n = unwords $ filter (not . null) $ reverse $ zipWith addScale scales chunks
  where
    chunks = breakIntoChunks n
    scales = ["", "thousand", "million", "billion", "trillion"]
    addScale scale chunk
      | chunk == 0 = ""
      | null scale = sayHundreds chunk
      | otherwise = sayHundreds chunk ++ " " ++ scale

breakIntoChunks :: Integer -> [Integer]
breakIntoChunks 0 = []
breakIntoChunks n = (n `mod` 1000) : breakIntoChunks (n `div` 1000)

sayHundreds :: Integer -> String
sayHundreds n
  | n == 0 = ""
  | n < 100 = sayTens n
  | n `mod` 100 == 0 = sayTens (n `div` 100) ++ " hundred"
  | otherwise = sayTens (n `div` 100) ++ " hundred " ++ sayTens (n `mod` 100)

sayTens :: Integer -> String
sayTens n
  | n < 20 = ones !! fromInteger n
  | n `mod` 10 == 0 = tens !! fromInteger (n `div` 10)
  | otherwise = tens !! fromInteger (n `div` 10) ++ "-" ++ ones !! fromInteger (n `mod` 10)
  where
    ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
            "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
            "seventeen", "eighteen", "nineteen"]
    tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
