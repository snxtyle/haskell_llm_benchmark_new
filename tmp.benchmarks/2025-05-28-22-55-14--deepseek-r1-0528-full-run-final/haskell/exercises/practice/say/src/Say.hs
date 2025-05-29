module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
    | n == 0 = Just "zero"
    | n < 0 || n > 999999999999 = Nothing
    | otherwise = Just (convert n)
  where
    convert n = unwords (filter (not . null) [billions, millions, thousands, units])
      where
        billions = convertChunk (n `div` 1000000000) "billion"
        millions = convertChunk ((n `mod` 1000000000) `div` 1000000) "million"
        thousands = convertChunk ((n `mod` 1000000) `div` 1000) "thousand"
        units = convertChunk (n `mod` 1000) ""
    
    convertChunk :: Integer -> String -> String
    convertChunk 0 _ = ""
    convertChunk n scale = 
        let chunkStr = convertThreeDigit n 
        in if null scale then chunkStr else chunkStr ++ " " ++ scale
    
    convertThreeDigit :: Integer -> String
    convertThreeDigit n
        | n >= 100 = 
            let hundreds = n `div` 100
                remainder = n `mod` 100
            in convertDigit hundreds ++ " hundred" ++ 
               if remainder /= 0 
                  then " " ++ convertTwoDigit remainder 
                  else ""
        | otherwise = convertTwoDigit n
    
    convertTwoDigit :: Integer -> String
    convertTwoDigit n
        | n < 10 = convertDigit n
        | n < 20 = case n of
            10 -> "ten"
            11 -> "eleven"
            12 -> "twelve"
            13 -> "thirteen"
            14 -> "fourteen"
            15 -> "fifteen"
            16 -> "sixteen"
            17 -> "seventeen"
            18 -> "eighteen"
            19 -> "nineteen"
        | otherwise = 
            let tens = n `div` 10
                units = n `mod` 10
                tensWord = case tens of
                    2 -> "twenty"
                    3 -> "thirty"
                    4 -> "forty"
                    5 -> "fifty"
                    6 -> "sixty"
                    7 -> "seventy"
                    8 -> "eighty"
                    9 -> "ninety"
            in if units == 0
                then tensWord
                else tensWord ++ "-" ++ convertDigit units
    
    convertDigit :: Integer -> String
    convertDigit n = case n of
        0 -> ""
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
