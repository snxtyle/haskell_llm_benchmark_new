{-# LANGUAGE NumericUnderscores #-}
module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999_999_999_999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ unwords $ concat
      [ segment billions "billion"
      , segment millions "million"
      , segment thousands "thousand"
      , lastSegment hundreds
      ]
  where
    billions  = n `div` 1_000_000_000
    millions  = (n `div` 1_000_000) `mod` 1000
    thousands = (n `div` 1_000) `mod` 1000
    hundreds  = n `mod` 1000

    segment :: Integer -> String -> [String]
    segment q name
      | q == 0    = []
      | otherwise = [sayHundreds q ++ " " ++ name]

    lastSegment :: Integer -> [String]
    lastSegment r
      | r == 0    = []
      | otherwise = [sayHundreds r]

sayHundreds :: Integer -> String
sayHundreds x
  | x < 100   = sayBelow100 x
  | otherwise =
      let h = x `div` 100
          r = x `mod` 100
          base = smallWord h ++ " " ++ "hundred"
      in if r == 0 then base else base ++ " " ++ sayBelow100 r

sayBelow100 :: Integer -> String
sayBelow100 x
  | x < 20   = smallWord x
  | otherwise =
      let t = x `div` 10
          u = x `mod` 10
          tens = tensWord t
      in if u == 0 then tens else tens ++ "-" ++ smallWord u

smallWord :: Integer -> String
smallWord 0  = "zero"
smallWord 1  = "one"
smallWord 2  = "two"
smallWord 3  = "three"
smallWord 4  = "four"
smallWord 5  = "five"
smallWord 6  = "six"
smallWord 7  = "seven"
smallWord 8  = "eight"
smallWord 9  = "nine"
smallWord 10 = "ten"
smallWord 11 = "eleven"
smallWord 12 = "twelve"
smallWord 13 = "thirteen"
smallWord 14 = "fourteen"
smallWord 15 = "fifteen"
smallWord 16 = "sixteen"
smallWord 17 = "seventeen"
smallWord 18 = "eighteen"
smallWord 19 = "nineteen"
smallWord _  = error "smallWord: input out of range (expected 0..19)"

tensWord :: Integer -> String
tensWord 2 = "twenty"
tensWord 3 = "thirty"
tensWord 4 = "forty"
tensWord 5 = "fifty"
tensWord 6 = "sixty"
tensWord 7 = "seventy"
tensWord 8 = "eighty"
tensWord 9 = "ninety"
tensWord _ = error "tensWord: input out of range (expected 2..9)"
