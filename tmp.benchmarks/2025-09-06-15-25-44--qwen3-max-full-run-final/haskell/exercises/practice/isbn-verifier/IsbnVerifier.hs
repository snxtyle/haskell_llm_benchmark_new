module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn str = 
    let cleanStr = filter (/= '-') str
    in if length cleanStr /= 10
       then False
       else let digits = take 9 cleanStr
                checkDigit = last cleanStr
            in all isDigit digits && 
               (isDigit checkDigit || checkDigit == 'X') &&
               calculateChecksum cleanStr == 0

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

charToValue :: Char -> Int
charToValue 'X' = 10
charToValue c = read [c]

calculateChecksum :: String -> Int
calculateChecksum str = 
    let values = map charToValue str
        weights = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
        weightedSum = sum $ zipWith (*) values weights
    in weightedSum `mod` 11
