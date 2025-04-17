module Say (inEnglish) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- | Convert an Integer in the range 0 – 999 999 999 999 to its
--   lowercase English representation.  Returns Nothing for any value
--   outside that range.
--
--   Examples:
--
--   >>> inEnglish 22
--   Just "twenty-two"
--
--   >>> inEnglish 12345
--   Just "twelve thousand three hundred forty-five"
--
--   >>> inEnglish (-3)
--   Nothing
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > maxAllowed = Nothing
  | n == 0                  = Just "zero"
  | otherwise               = Just . intercalate " " . reverse $
      mapMaybe chunkToWords (zip chunks scaleWords)
  where
    -- maximum allowed value (999,999,999,999)
    maxAllowed :: Integer
    maxAllowed = 999999999999
    -- break the number into 3‑digit chunks starting from the right
    chunks :: [Int]
    chunks = go n
      where
        go 0 = []
        go x = fromIntegral (x `mod` 1000) : go (x `div` 1000)

    scaleWords :: [String]
    scaleWords =
      [ ""          -- 10^0  (ones)
      , "thousand"  -- 10^3
      , "million"   -- 10^6
      , "billion"   -- 10^9
      , "trillion"  -- 10^12  (upper bound)
      ]

    -- Turn a (chunk, scale-name) pair into its textual form,
    -- or Nothing when the chunk is zero (so it will be skipped).
    chunkToWords :: (Int, String) -> Maybe String
    chunkToWords (0, _)      = Nothing
    chunkToWords (c, scale)  =
      let body = threeDigits c
      in Just $ if null scale then body else body ++ " " ++ scale

--------------------------------------------------------------------
-- Helpers for sub‑1000 conversions
--------------------------------------------------------------------

-- | Convert 1–999 to English words.
threeDigits :: Int -> String
threeDigits n
  | n >= 100 =
      let (h, r) = n `divMod` 100
          hundredsPart = unitWords !! h ++ " hundred"
      in if r == 0
            then hundredsPart
            else hundredsPart ++ " " ++ twoDigits r
  | otherwise = twoDigits n

-- | Convert 1–99 to English words.
twoDigits :: Int -> String
twoDigits n
  | n < 20    = unitWords !! n
  | otherwise =
      let (t, u) = n `divMod` 10
          tensPart = tensWords !! t
      in if u == 0
            then tensPart
            else tensPart ++ "-" ++ unitWords !! u

unitWords :: [String]
unitWords =
  [ "zero", "one", "two", "three", "four"
  , "five", "six", "seven", "eight", "nine"
  , "ten", "eleven", "twelve", "thirteen", "fourteen"
  , "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  ]

-- index 0 unused so that 2 → "twenty", 3 → "thirty", etc.
tensWords :: [String]
tensWords =
  [ ""         -- placeholder for 0
  , ""         -- 10 is handled in unitWords
  , "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]
