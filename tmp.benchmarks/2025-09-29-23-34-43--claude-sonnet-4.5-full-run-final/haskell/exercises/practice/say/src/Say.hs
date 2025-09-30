module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ sayNumber n

sayNumber :: Integer -> String
sayNumber n
  | n < 20 = ones !! fromInteger n
  | n < 100 = let (tens', ones') = n `divMod` 10
                  tensWord = tens !! fromInteger (tens' - 2)
              in if ones' == 0
                 then tensWord
                 else tensWord ++ "-" ++ ones !! fromInteger ones'
  | n < 1000 = sayHundreds n
  | n < 1000000 = sayWithScale n 1000 "thousand"
  | n < 1000000000 = sayWithScale n 1000000 "million"
  | otherwise = sayWithScale n 1000000000 "billion"

sayHundreds :: Integer -> String
sayHundreds n =
  let (hundreds', rest) = n `divMod` 100
      hundredsWord = ones !! fromInteger hundreds' ++ " hundred"
  in if rest == 0
     then hundredsWord
     else hundredsWord ++ " " ++ sayNumber rest

sayWithScale :: Integer -> Integer -> String -> String
sayWithScale n scale scaleName =
  let (high, low) = n `divMod` scale
      highPart = sayNumber high ++ " " ++ scaleName
  in if low == 0
     then highPart
     else highPart ++ " " ++ sayNumber low

ones :: [String]
ones = [ "zero", "one", "two", "three", "four", "five", "six", "seven",
         "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
         "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

tens :: [String]
tens = [ "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
         "eighty", "ninety" ]
