module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map verse [start .. stop]

verse :: Int -> String
verse d =
  "On the " ++ ordinal d ++ " day of Christmas my true love gave to me: " ++ giftsFor d

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

giftsFor :: Int -> String
giftsFor 1 = head gifts
giftsFor n = intercalate ", " (map gift [n, n - 1 .. 2]) ++ ", and " ++ head gifts
  where
    gift i = gifts !! (i - 1)

gifts :: [String]
gifts =
  [ "a Partridge in a Pear Tree."
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
