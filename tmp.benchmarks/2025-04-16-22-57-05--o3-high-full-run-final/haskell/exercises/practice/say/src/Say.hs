{-# LANGUAGE NumericUnderscores #-}

module Say (inEnglish) where

import Data.List (intercalate)

-- | Convert an integer in the range 0‑999,999,999,999 to its English
--   representation.  Out‑of‑range inputs yield Nothing.
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999_999_999_999 = Nothing
  | n == 0                       = Just "zero"
  | otherwise                    = Just $ intercalate " " $ filter (not . null) segments
  where
    (billions,  remBillions)  = n `divMod` 1_000_000_000
    (millions,  remMillions)  = remBillions `divMod` 1_000_000
    (thousands, rest)         = remMillions `divMod` 1_000

    segments =
      concat
        [ if billions  > 0 then [spellChunk billions  ++ " billion"]  else []
        , if millions  > 0 then [spellChunk millions  ++ " million"]  else []
        , if thousands > 0 then [spellChunk thousands ++ " thousand"] else []
        , if rest      > 0 then [spellChunk rest]                     else []
        ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- Convert a number 1‥999 (inclusive) to English words.
spellChunk :: Integer -> String
spellChunk = spellUnder1000 . fromIntegral

-- Handles numbers < 1000.
spellUnder1000 :: Int -> String
spellUnder1000 n
  | n >= 100  =
      let hundredsDigit = n `div` 100
          remainder     = n `mod` 100
          prefix        = under20 !! hundredsDigit ++ " hundred"
      in if remainder == 0
           then prefix
           else prefix ++ " " ++ spellUnder100 remainder
  | otherwise = spellUnder100 n

-- Handles numbers < 100.
spellUnder100 :: Int -> String
spellUnder100 n
  | n < 20    = under20 !! n
  | otherwise =
      let tensPart = tens !! (n `div` 10)
          ones      = n `mod` 10
      in if ones == 0
           then tensPart
           else tensPart ++ "-" ++ under20 !! ones

-- Word lists --------------------------------------------------------

under20 :: [String]
under20 =
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

-- Index 0 and 1 are unused because 0‑19 are covered by 'under20'.
tens :: [String]
tens =
  [ ""        -- 0
  , ""        -- 1
  , "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]
