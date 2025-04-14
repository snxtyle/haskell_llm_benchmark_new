module TwelveDays (recite) where

import Data.List (intercalate)

-- List of ordinal numbers for days 1 through 12
ordinals :: [String]
ordinals = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

-- List of gifts for days 1 through 12
gifts :: [String]
gifts = [
  "a Partridge in a Pear Tree.",
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

-- Function to generate a single verse for a given day
verse :: Int -> String
verse n
  | n >= 1 && n <= 12 = "On the " ++ ordinals !! (n - 1) ++ " day of Christmas my true love gave to me: " ++ giftsString n
  | otherwise = error "Day must be between 1 and 12."
  where
    lst = reverse (take n gifts)  -- Gifts for day n down to 1
    giftsString n
      | n == 1    = head lst  -- Just the first gift
      | otherwise = intercalate ", " (init lst) ++ ", and " ++ last lst  -- Comma-separated with "and" before the last

recite :: Int -> Int -> [String]
recite start stop
  | start >= 1 && stop <= 12 && start <= stop = [verse n | n <- [start..stop]]
  | otherwise = error "Start and stop must satisfy: 1 <= start <= stop <= 12."
