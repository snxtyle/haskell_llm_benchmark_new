module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n < 100 = Just (spellTwoDigits n)
  | n < 1000 = Just (spellThreeDigits n)
  | otherwise = Nothing

spellTwoDigits :: Integer -> String
spellTwoDigits n
  | n < 20 = units !! fromIntegral n
  | otherwise =
      let (tensDigit, unitDigit) = n `divMod` 10
          tensPart = tens !! fromIntegral (tensDigit - 2)
          unitPart = units !! fromIntegral unitDigit
      in if unitDigit == 0 then tensPart else tensPart ++ "-" ++ unitPart

spellThreeDigits :: Integer -> String
spellThreeDigits n =
  let (hundredsDigit, rest) = n `divMod` 100
      hundredsPart = if hundredsDigit == 0 then "" else units !! fromIntegral hundredsDigit ++ " hundred"
      restPart = if rest == 0 then "" else spellTwoDigits rest
  in if hundredsDigit /= 0 && rest /= 0
       then hundredsPart ++ " " ++ restPart
       else hundredsPart ++ restPart

units :: [String]
units =
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  , "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eighteen"
  , "nineteen"
  ]

tens :: [String]
tens =
  [ "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]
