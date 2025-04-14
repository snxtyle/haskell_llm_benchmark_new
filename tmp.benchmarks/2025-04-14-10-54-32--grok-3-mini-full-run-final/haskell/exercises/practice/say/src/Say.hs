module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing  -- Out of range
  | otherwise = Just (toEnglish n)

-- Helper lists
ones :: [String]
ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
        "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"]  -- Up to trillion for 10^12

-- Convert a number from 0 to 999 to English
convertBelowThousand :: Integer -> String
convertBelowThousand 0 = "zero"
convertBelowThousand n
  | n < 20 = ones !! fromIntegral n
  | n < 100 = let tensPart = tens !! fromIntegral (n `div` 10)
                  onesPart = if n `mod` 10 > 0 then "-" ++ convertBelowThousand (n `mod` 10) else ""
              in tensPart ++ onesPart
  | n < 1000 = let hundredsPart = ones !! fromIntegral (n `div` 100) ++ " hundred"
                   restPart = if n `mod` 100 > 0 then " " ++ convertBelowThousand (n `mod` 100) else ""
               in hundredsPart ++ restPart

-- Get chunks of three digits from the number
getChunks :: Integer -> [Integer]
getChunks 0 = [0]
getChunks n = reverse $ go n
  where
    go x
      | x > 0 = (x `mod` 1000) : go (x `div` 1000)
      | otherwise = []

-- Main conversion function
toEnglish :: Integer -> String
toEnglish 0 = "zero"
toEnglish n = unwords $ filter (not . null) $ reverse $ zipWithMaybe addScale (getChunks n) scales
  where
    zipWithMaybe f xs ys = [f x y | (x, y) <- zip xs ys, x > 0]
    addScale chunk scale = if scale /= "" then convertBelowThousand chunk ++ " " ++ scale else convertBelowThousand chunk
