module Say (inEnglish) where

-- Words for 0–19
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

-- Tens names for 20,30,…,90
tens :: [String]
tens =
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

-- Convert 0–99
under100 :: Integer -> String
under100 n
  | n < 20 = units !! fromIntegral n
  | otherwise =
    let t = tens !! fromIntegral (n `div` 10)
        u = n `mod` 10
     in if u == 0 then t else t ++ "-" ++ units !! fromIntegral u

-- Convert 0–999
under1000 :: Integer -> String
under1000 n
  | n < 100 = under100 n
  | otherwise =
    let (h, r) = n `divMod` 100
        prefix = units !! fromIntegral h ++ " hundred"
     in if r == 0 then prefix else prefix ++ " " ++ under100 r

-- Scale labels
scales :: [(Integer, String)]
scales =
  [ (10 ^ 9, "billion")
  , (10 ^ 6, "million")
  , (10 ^ 3, "thousand")
  , (1, "")
  ]

-- Build full English phrase for 1–999,999,999,999
spell :: Integer -> String
spell n = unwords (go n scales)
  where
    go 0 _ = []
    go _ [] = []
    go x ((value, name) : rest) =
      let (count, rem') = x `divMod` value
          partWords =
            if count > 0
              then
                let chunk = under1000 count
                    suffix = if name /= "" then " " ++ name else ""
                 in [chunk ++ suffix]
              else []
       in partWords ++ go rem' rest

-- Public API
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (spell n)
