module Phone (number) where

number :: String -> Maybe String
number xs = 
  let cleaned = filter (`elem` ['0'..'9']) xs
      len = length cleaned
  in case () of
    _
      | len == 10 && isValidNANP cleaned -> Just cleaned
      | len == 11 && head cleaned == '1' && isValidNANP (tail cleaned) -> Just (tail cleaned)
      | otherwise -> Nothing
  where
    isValidNANP s = 
      let areaCode = take 3 s
          exchangeCode = take 3 (drop 3 s)
      in head areaCode `elem` ['2'..'9'] && head exchangeCode `elem` ['2'..'9']
