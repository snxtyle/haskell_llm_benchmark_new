module TwelveDays (recite) where

import           Data.List (intercalate)

-- Public API ----------------------------------------------------------

-- | Produce the verses from @start@ to @stop@ (inclusive).
--
--   Each verse is returned as a separate String element.
--
--   Examples:
--
--   >>> recite 1 1
--   ["On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."]
--
--   >>> recite 1 2
--   [ "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
--   , "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree."
--   ]
recite :: Int -> Int -> [String]
recite start stop = map verse [start .. stop]

-- Internals -----------------------------------------------------------

verse :: Int -> String
verse n =
  "On the "
    ++ ordinal !! (n - 1)
    ++ " day of Christmas my true love gave to me: "
    ++ giftsFor n

giftsFor :: Int -> String
giftsFor 1 = firstGift
giftsFor n =
  intercalate " " (map giftDesc [n, n -1 .. 2] ++ ["and " ++ firstGift])
  where
    giftDesc day = gifts !! (day - 1)

firstGift :: String
firstGift = gifts !! 0

ordinal :: [String]
ordinal =
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

gifts :: [String]
gifts =
  [ "a Partridge in a Pear Tree."
  , "two Turtle Doves,"
  , "three French Hens,"
  , "four Calling Birds,"
  , "five Gold Rings,"
  , "six Geese-a-Laying,"
  , "seven Swans-a-Swimming,"
  , "eight Maids-a-Milking,"
  , "nine Ladies Dancing,"
  , "ten Lords-a-Leaping,"
  , "eleven Pipers Piping,"
  , "twelve Drummers Drumming,"
  ]
