module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn s = 
  let cleaned = filter (/= '-') s
  in isValidLength cleaned && isValidFormat cleaned && isValidChecksum cleaned

isValidLength :: String -> Bool
isValidLength s = length s == 10

isValidFormat :: String -> Bool
isValidFormat s = 
  let (digits, check) = splitAt 9 s
  in all (`elem` ['0'..'9']) digits && (check == "" || head check `elem` ['0'..'9'] ++ ['X'])

isValidChecksum :: String -> Bool
isValidChecksum s = 
  let digits = take 9 s
      checkChar = last s
      checkValue = if checkChar == 'X' then 10 else read [checkChar]
      weights = [10,9,8,7,6,5,4,3,2,1]
      values = map (\c -> read [c] :: Int) digits ++ [checkValue]
      total = sum $ zipWith (*) values weights
  in total `mod` 11 == 0
