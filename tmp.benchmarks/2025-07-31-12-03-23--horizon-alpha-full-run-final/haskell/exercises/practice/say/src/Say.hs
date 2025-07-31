module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > maxAllowed = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (spell n)
  where
    maxAllowed = 999999999999

spell :: Integer -> String
spell n =
  let chunks = toChunks n
      parts = zipWith spellChunk (reverse chunks) scales
      nonEmpty = filter (not . null) parts
  in intercalate " " nonEmpty

-- Break number into chunks of three digits from least significant side
toChunks :: Integer -> [Integer]
toChunks 0 = [0]
toChunks x = reverse (go x)
  where
    go 0 = []
    go y = let (q, r) = y `divMod` 1000 in r : go q

-- Scale names aligned with chunk order from most significant to least
-- For example: [trillion, billion, million, thousand, ""]
scales :: [String]
scales = ["trillion","billion","million","thousand",""]

spellChunk :: Integer -> String -> String
spellChunk 0 _ = ""
spellChunk n scale =
  let words3 = spellUnder1000 n
  in if null scale then words3 else words3 ++ " " ++ scale

spellUnder1000 :: Integer -> String
spellUnder1000 n
  | n < 100 = spellUnder100 n
  | otherwise =
      let (h, r) = n `divMod` 100
          hundredPart = smallNumbers !! fromIntegral h ++ " hundred"
      in if r == 0 then hundredPart else hundredPart ++ " " ++ spellUnder100 r

spellUnder100 :: Integer -> String
spellUnder100 n
  | n < 20 = smallNumbers !! fromIntegral n
  | otherwise =
      let (t, r) = n `divMod` 10
          tensWord = tensNumbers !! fromIntegral t
      in if r == 0 then tensWord else tensWord ++ "-" ++ smallNumbers !! fromIntegral r

smallNumbers :: [String]
smallNumbers =
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

tensNumbers :: [String]
tensNumbers =
  [ ""
  , ""
  , "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]
