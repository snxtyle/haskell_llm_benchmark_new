{-# LANGUAGE NumericUnderscores #-}
module Say (inEnglish) where

-- | Convert an integer in the range 0‒999,999,999,999 into its English
-- representation.
--   Returns Nothing for numbers outside that range.
--
--   Examples
--   >>> inEnglish 0
--   Just "zero"
--   >>> inEnglish 12345
--   Just "twelve thousand three hundred forty-five"
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > maxNumber = Nothing
  | n == 0                 = Just "zero"
  | otherwise              = Just (unwords . filter (not . null) $ parts)
  where
    maxNumber = 999_999_999_999

    parts =
      [ chunk billions "billion"
      , chunk millions "million"
      , chunk thousands "thousand"
      , belowThousand remainder
      ]

    (billions, millions, thousands, remainder) =
      ( n `div` 1_000_000_000
      , (n `div` 1_000_000) `mod` 1000
      , (n `div` 1000) `mod` 1000
      , n `mod` 1000
      )

    chunk :: Integer -> String -> String
    chunk 0 _       = ""
    chunk value word = belowThousand value ++ " " ++ word

-- Convert a number 0‒999 to English.
-- Returns the empty string for 0 so that the caller can easily drop
-- leading zero-chunks while still writing e.g. “zero” for the whole number.
belowThousand :: Integer -> String
belowThousand num
  | num == 0  = ""
  | num < 100 = belowHundred num
  | otherwise =
      let h  = num `div` 100
          rs = num `mod` 100
          hundredsPart = unitWords !! fromInteger h ++ " hundred"
          restPart     = if rs == 0 then "" else " " ++ belowHundred rs
      in hundredsPart ++ restPart

-- Convert a number 0‒99 to English.
belowHundred :: Integer -> String
belowHundred n
  | n < 20    = unitWords !! fromInteger n
  | otherwise =
      let t       = n `div` 10
          ones    = n `mod` 10
          tensStr = tensWords !! fromInteger (t - 2)
      in if ones == 0
           then tensStr
           else tensStr ++ "-" ++ unitWords !! fromInteger ones

unitWords :: [String]
unitWords =
  [ "zero", "one", "two", "three", "four"
  , "five", "six", "seven", "eight", "nine"
  , "ten", "eleven", "twelve", "thirteen", "fourteen"
  , "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  ]

tensWords :: [String]
tensWords =
  [ "twenty", "thirty", "forty", "fifty"
  , "sixty", "seventy", "eighty", "ninety"
  ]
