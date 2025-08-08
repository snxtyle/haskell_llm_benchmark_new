module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0                    = Just "zero"
  | otherwise                 = Just (numberToWords n)

-- Helpers

numberToWords :: Integer -> String
numberToWords n =
  let cs    = chunks n
      names = reverse (take (length cs) scales)
      pairs = zip cs names
      parts =
        [ if name == ""
            then chunkToWords chunk
            else chunkToWords chunk ++ " " ++ name
        | (chunk, name) <- pairs
        , chunk /= 0
        ]
  in unwords parts

chunks :: Integer -> [Integer]
chunks 0 = []
chunks x =
  let (q, r) = x `divMod` 1000
  in chunks q ++ [r]

chunkToWords :: Integer -> String
chunkToWords n =
  let h = n `div` 100
      r = n `mod` 100
      hundredPart = if h == 0 then "" else smallNumbers !! fromIntegral h ++ " hundred"
      restPart    = if r == 0 then "" else englishUnder100 r
  in case (hundredPart, restPart) of
       ("", rp) -> rp
       (hp, "") -> hp
       (hp, rp) -> hp ++ " " ++ rp

englishUnder100 :: Integer -> String
englishUnder100 n
  | n < 20   = smallNumbers !! fromIntegral n
  | otherwise =
      let t = fromIntegral (n `div` 10) :: Int
          u = fromIntegral (n `mod` 10) :: Int
          tensPart = tensWords !! (t - 2)
      in if u == 0
           then tensPart
           else tensPart ++ "-" ++ smallNumbers !! u

scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"]

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

tensWords :: [String]
tensWords =
  [ "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]
