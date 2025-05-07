module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]

verse :: Int -> String
verse n =
    "On the " ++ ordinal n ++ " day of Christmas my true love gave to me: " ++ gifts n ++ "."

ordinal :: Int -> String
ordinal n = ordinals !! (n - 1)
  where
    ordinals =
      [ "first"
      , "second"
      , "third"
      , "fourth"
      , "fifth"
      , "sixth"
      , "seventh"
      , "eighth"
      , "ninth"
      , "tenth"
      , "eleventh"
      , "twelfth"
      ]

gifts :: Int -> String
gifts n
  | n == 1    = giftList !! 0
  | otherwise = concatGifts n

concatGifts :: Int -> String
concatGifts n =
    let gs = map (\i -> giftList !! (i - 1)) [n, n-1 .. 1]
        (firsts, lastOne) = splitAt (length gs - 1) gs
    in case firsts of
         [] -> lastOne !! 0
         _  -> (concatMap (++ ", ") (init firsts)) ++ (if null firsts then "" else firsts !! (length firsts - 1) ++ ", and ") ++ lastOne !! 0

giftList :: [String]
giftList =
  [ "a Partridge in a Pear Tree"
  , "two Turtle Doves"
  , "three French Hens"
  , "four Calling Birds"
  , "five Gold Rings"
  , "six Geese-a-Laying"
  , "seven Swans-a-Swimming"
  , "eight Maids-a-Milking"
  , "nine Ladies Dancing"
  , "ten Lords-a-Leaping"
  , "eleven Pipers Piping"
  , "twelve Drummers Drumming"
  ]
