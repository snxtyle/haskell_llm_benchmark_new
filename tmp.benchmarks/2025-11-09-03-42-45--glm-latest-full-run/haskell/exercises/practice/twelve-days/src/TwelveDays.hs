module TwelveDays (recite) where

import Data.List (intercalate)

ordinals :: [String]
ordinals = [ "first"
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

gifts :: [String]
gifts = [ "twelve Drummers Drumming"
        , "eleven Pipers Piping"
        , "ten Lords-a-Leaping"
        , "nine Ladies Dancing"
        , "eight Maids-a-Milking"
        , "seven Swans-a-Swimming"
        , "six Geese-a-Laying"
        , "five Gold Rings"
        , "four Calling Birds"
        , "three French Hens"
        , "two Turtle Doves"
        , "a Partridge in a Pear Tree"
        ]

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]

verse :: Int -> String
verse n =
  let ordinal = ordinals !! (n - 1)
      giftsForDay = drop (length gifts - n) gifts
      giftsStr = formatGifts giftsForDay
  in "On the " ++ ordinal ++ " day of Christmas my true love gave to me: " ++ giftsStr ++ "."

formatGifts :: [String] -> String
formatGifts [] = ""
formatGifts [x] = x
formatGifts xs = intercalate ", " (init xs) ++ ", and " ++ last xs
