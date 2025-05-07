module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]

days :: [String]
days =
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

verse :: Int -> String
verse n =
  "On the " ++ days !! (n - 1)
  ++ " day of Christmas my true love gave to me: "
  ++ giftLines n

giftLines :: Int -> String
giftLines 1 = gifts !! 0 ++ "."
giftLines n =
  intercalate ", "
    (map (gifts !!) (reverse [1 .. n - 1]) ++ ["and " ++ gifts !! 0 ++ "."])
