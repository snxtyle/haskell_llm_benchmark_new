module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 || n > 999999999999 = Nothing
    | n == 0 = Just "zero"
    | otherwise = Just (convert n)

convert :: Integer -> String
convert n
    | n < 0 = error "Negative number"
    | n < 20 = ones !! fromIntegral n
    | n < 100 = 
        let (tensDigit, onesDigit) = n `divMod` 10
        in if onesDigit == 0
            then tens !! fromIntegral tensDigit
            else tens !! fromIntegral tensDigit ++ "-" ++ ones !! fromIntegral onesDigit
    | n < 1000 =
        let (hundreds, remainder) = n `divMod` 100
        in if remainder == 0
            then ones !! fromIntegral hundreds ++ " hundred"
            else ones !! fromIntegral hundreds ++ " hundred " ++ convert remainder
    | otherwise =
        let scales = ["", "thousand", "million", "billion"]
            chunks = breakIntoChunks n
            chunkTexts = map convert chunks
            scaleWords = reverse $ take (length chunks) (drop 1 scales)  -- Skip empty first element
            combined = zipWith (\text scale -> if text /= "zero" then text ++ " " ++ scale else "") chunkTexts scaleWords
        in unwords (filter (not . null) combined)

breakIntoChunks :: Integer -> [Integer]
breakIntoChunks n = go n []
    where
        go 0 acc = acc
        go num acc =
            let (quotient, remainder) = num `divMod` 1000
            in go quotient (remainder : acc)

ones :: [String]
ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
        "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
