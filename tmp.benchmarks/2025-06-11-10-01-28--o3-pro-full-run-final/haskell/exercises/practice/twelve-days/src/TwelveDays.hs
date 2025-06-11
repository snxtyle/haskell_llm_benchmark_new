module TwelveDays (recite) where

import Data.List (intercalate)

-- | Produce the requested verses (inclusive) of
--   "The Twelve Days of Christmas".
--
--   The first argument is the starting verse number (1-based),
--   the second argument is the ending verse number.
--
--   Each element of the resulting list is an entire verse.
recite :: Int -> Int -> [String]
recite start stop = map verse [start .. stop]

-- Internal helpers -----------------------------------------------------------

verse :: Int -> String
verse n = opening n ++ giftsLine n

opening :: Int -> String
opening n =
  "On the "
    ++ ordinals !! (n - 1)
    ++ " day of Christmas my true love gave to me: "

giftsLine :: Int -> String
giftsLine 1 = gifts !! 0                           -- base case (no “and”)
giftsLine n =
  intercalate ", "
    (map giftPhrase (reverse [1 .. n]))
  where
    giftPhrase 1 = "and " ++ gifts !! 0            -- prepend “and” for last gift
    giftPhrase k = gifts !! (k - 1)

-- Constant data --------------------------------------------------------------

ordinals :: [String]
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
