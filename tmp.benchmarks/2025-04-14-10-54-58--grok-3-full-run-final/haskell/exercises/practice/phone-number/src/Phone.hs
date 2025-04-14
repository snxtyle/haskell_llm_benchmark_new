module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let digits = filter isDigit xs
    in case length digits of
        10 -> validate digits
        11 -> if head digits == '1' then validate (tail digits) else Nothing
        _ -> Nothing
  where
    validate ds = 
        let areaCode = take 3 ds
            exchangeCode = take 3 (drop 3 ds)
        in if isValidCode areaCode && isValidCode exchangeCode
           then Just ds
           else Nothing
    isValidCode (x:_) = x `elem` ['2'..'9']
    isValidCode _ = False
