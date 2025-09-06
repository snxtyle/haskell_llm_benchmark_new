module TwelveDays (recite) where

import Data.List (intersperse)

ordinals :: [String]
ordinals = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

gifts :: [String]
gifts = [
    "a Partridge in a Pear Tree",
    "two Turtle Doves",
    "three French Hens",
    "four Calling Birds",
    "five Gold Rings",
    "six Geese-a-Laying",
    "seven Swans-a-Swimming",
    "eight Maids-a-Milking",
    "nine Ladies Dancing",
    "ten Lords-a-Leaping",
    "eleven Pipers Piping",
    "twelve Drummers Drumming"
    ]

verse :: Int -> String
verse n = "On the " ++ ordinals !! (n - 1) ++ " day of Christmas my true love gave to me: " ++ giftsString n
  where
    giftsString 1 = gifts !! 0
    giftsString k = concat (intersperse ", " (map (gifts !!) [k-1, k-2 .. 1])) ++ ", and " ++ gifts !! 0

recite :: Int -> Int -> [String]
recite start stop
  | start > stop = []
  | otherwise = map verse [start .. stop]
