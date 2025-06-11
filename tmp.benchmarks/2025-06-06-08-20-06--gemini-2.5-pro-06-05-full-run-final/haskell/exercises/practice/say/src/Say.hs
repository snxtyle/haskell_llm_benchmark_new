module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n >= 1000000000000 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (say' n)
  where
    ones :: [String]
    ones = [ "zero"
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
    tens = [ ""
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

    scales :: [String]
    scales = ["", "thousand", "million", "billion", "trillion"]

    sayUnder100 :: Integer -> String
    sayUnder100 num
      | num < 20 = ones !! fromIntegral num
      | otherwise =
        let (t, u) = num `divMod` 10
        in case u of
             0 -> tens !! fromIntegral t
             _ -> tens !! fromIntegral t ++ "-" ++ (ones !! fromIntegral u)

    sayUnder1000 :: Integer -> String
    sayUnder1000 num
      | num < 100 = sayUnder100 num
      | otherwise =
        let (h, rest) = num `divMod` 100
        in case rest of
             0 -> ones !! fromIntegral h ++ " hundred"
             _ -> ones !! fromIntegral h ++ " hundred " ++ sayUnder100 rest

    getChunks :: Integer -> [Integer]
    getChunks 0 = []
    getChunks num = num `mod` 1000 : getChunks (num `div` 1000)

    say' :: Integer -> String
    say' num =
      let chunks = getChunks num
          parts = zipWith toPart chunks scales
          nonEmpty = filter (not . null) parts
      in intercalate " " (reverse nonEmpty)
      where
        toPart :: Integer -> String -> String
        toPart 0 _ = ""
        toPart chunk scale
          | null scale = sayUnder1000 chunk
          | otherwise  = sayUnder1000 chunk ++ " " ++ scale
