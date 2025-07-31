module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (convertNumber n)

convertNumber :: Integer -> String
convertNumber n = unwords $ filter (not . null) (convertChunks chunks scales)
  where
    chunks = getChunks n
    scales = ["", "thousand", "million", "billion"]

getChunks :: Integer -> [Integer]
getChunks 0 = [0]
getChunks n = reverse $ getChunks' n []
  where
    getChunks' 0 acc = acc
    getChunks' num acc = getChunks' (num `div` 1000) ((num `mod` 1000) : acc)

convertChunks :: [Integer] -> [String] -> [String]
convertChunks [] _ = []
convertChunks _ [] = []
convertChunks (0:xs) (_:scales') = convertChunks xs scales'
convertChunks (x:xs) (s:sc) = 
  let chunkStr = convertChunk (fromIntegral x)
      result = if null s 
               then chunkStr
               else if null chunkStr
                    then ""
                    else chunkStr ++ " " ++ s
  in result : convertChunks xs sc

convertChunk :: Int -> String
convertChunk n
  | n < 0 || n > 999 = ""
  | n == 0 = ""
  | n < 100 = convertTens n
  | otherwise = let hundreds = n `div` 100
                    remainder = n `mod` 100
                in convertDigit hundreds ++ " hundred" ++ 
                   (if remainder > 0 then " " ++ convertTens remainder else "")

convertTens :: Int -> String
convertTens n
  | n < 0 || n > 99 = ""
  | n < 20 = smallNumbers !! n
  | n `mod` 10 == 0 = tens !! (n `div` 10)
  | otherwise = tens !! (n `div` 10) ++ "-" ++ smallNumbers !! (n `mod` 10)

convertDigit :: Int -> String
convertDigit n
  | n < 0 || n > 9 = ""
  | otherwise = smallNumbers !! n

smallNumbers :: [String]
smallNumbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
