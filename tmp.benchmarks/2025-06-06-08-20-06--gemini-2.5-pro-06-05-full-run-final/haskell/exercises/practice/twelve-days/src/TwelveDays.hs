module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map reciteVerse [start..stop]

reciteVerse :: Int -> String
reciteVerse n =
    "On the " ++ (ordinals !! (n - 1)) ++ " day of Christmas my true love gave to me: " ++ giftsPhrase n

giftsPhrase :: Int -> String
giftsPhrase n = formatGifts . reverse . take n $ giftsByDay
  where
    formatGifts [g] = g ++ "."
    formatGifts gs = (intercalate ", " . init $ gs) ++ ", and " ++ last gs ++ "."

ordinals :: [String]
ordinals = ["first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

giftsByDay :: [String]
giftsByDay = [ "a Partridge in a Pear Tree",
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
