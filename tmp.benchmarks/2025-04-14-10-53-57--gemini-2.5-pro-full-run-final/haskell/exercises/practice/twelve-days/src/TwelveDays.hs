module TwelveDays (recite) where

import Data.List (intercalate)

-- Ordinal numbers for the days
ordinals :: [String]
ordinals = ["first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

-- Gifts for each day, indexed from 0 (day 1) to 11 (day 12)
gifts :: [String]
gifts = ["a Partridge in a Pear Tree", "two Turtle Doves", "three French Hens",
         "four Calling Birds", "five Gold Rings", "six Geese-a-Laying",
         "seven Swans-a-Swimming", "eight Maids-a-Milking", "nine Ladies Dancing",
         "ten Lords-a-Leaping", "eleven Pipers Piping", "twelve Drummers Drumming"]

-- Generates the string listing the gifts for a given day (n).
-- Lists gifts from day n down to day 1.
-- Example (n=3): "three French Hens, two Turtle Doves, and a Partridge in a Pear Tree"
giftsForDay :: Int -> String
giftsForDay n
    | n <= 0    = "" -- Should not happen with valid input 1-12
    | n == 1    = gifts !! 0 -- Special case for the first day, only one gift.
    | otherwise =
        -- Get the gifts for days 1 up to n. The gift for day `i` is at index `i-1`.
        let relevantGifts = take n gifts
        -- Reverse the list to get gifts in the order they appear in the song (day n down to day 1).
            giftsInVerseOrder = reverse relevantGifts
        -- Separate the last gift (which is always "a Partridge in a Pear Tree" for n > 1)
        -- to handle the "and" conjunction correctly.
            firstPart = init giftsInVerseOrder -- All gifts except the last one (day 1's gift)
            lastGift = last giftsInVerseOrder -- The gift for day 1
        -- Combine the parts with commas and "and".
        in intercalate ", " firstPart ++ ", and " ++ lastGift

-- Generates the full verse string for a given day (n).
verse :: Int -> String
verse n
  | n <= 0 || n > 12 = error $ "Invalid day number: " ++ show n -- Basic input validation
  | otherwise =
      -- Get the ordinal string (e.g., "first" for n=1) using 0-based indexing.
      let dayOrdinal = ordinals !! (n - 1)
          -- Construct the opening phrase of the verse.
          prefix = "On the " ++ dayOrdinal ++ " day of Christmas my true love gave to me: "
          -- Get the formatted gift list string for the current day.
          giftList = giftsForDay n
      -- Combine the prefix, the gift list, and the concluding period.
      in prefix ++ giftList ++ "."

-- Generates the list of verses from the start day to the stop day (inclusive).
recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop] -- Apply the verse function to each day in the range.
