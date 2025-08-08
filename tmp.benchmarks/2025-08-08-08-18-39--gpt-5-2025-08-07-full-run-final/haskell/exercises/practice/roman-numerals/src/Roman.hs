module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman n = snd $ foldl step (n, "") pairs
  where
    step :: (Integer, String) -> (Integer, String) -> (Integer, String)
    step (remaining, acc) (val, sym)
      | remaining == 0 = (0, acc)
      | otherwise =
          let count = fromIntegral (remaining `div` val) :: Int
              remaining' = remaining `mod` val
              acc' = acc ++ concat (replicate count sym)
          in (remaining', acc')

    pairs :: [(Integer, String)]
    pairs =
      [ (1000, "M")
      , (900,  "CM")
      , (500,  "D")
      , (400,  "CD")
      , (100,  "C")
      , (90,   "XC")
      , (50,   "L")
      , (40,   "XL")
      , (10,   "X")
      , (9,    "IX")
      , (5,    "V")
      , (4,    "IV")
      , (1,    "I")
      ]
