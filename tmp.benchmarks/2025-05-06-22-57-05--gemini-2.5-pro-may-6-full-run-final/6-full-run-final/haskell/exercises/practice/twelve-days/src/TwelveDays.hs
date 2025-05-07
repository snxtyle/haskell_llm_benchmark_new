module TwelveDays (recite) where

import Data.List (intercalate)

-- Ordinal numbers for the days, e.g., "first", "second".
dayOrdinals :: [String]
dayOrdinals =
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

-- Gifts for each day. The index corresponds to (day - 1).
allGifts :: [String]
allGifts =
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

-- Formats the list of presents for a given day's verse.
-- Example: ["two Turtle Doves", "a Partridge in a Pear Tree"]
-- Becomes: "two Turtle Doves, and a Partridge in a Pear Tree."
formatPresents :: [String] -> String
formatPresents presents
  | null presents = "." -- Should ideally not be reached with valid day numbers.
  | length presents == 1 = head presents ++ "."
  | otherwise =
      -- 'presents' are already in the order they should be sung:
      -- e.g., for day 3: ["three French Hens", "two Turtle Doves", "a Partridge in a Pear Tree"]
      let
        giftsToJoin = init presents -- All gifts except the last one ("a Partridge...")
        lastGift = last presents    -- The "a Partridge in a Pear Tree" gift
      in
        intercalate ", " giftsToJoin ++ ", and " ++ lastGift ++ "."

-- Generates the full verse for a single day.
verse :: Int -> String
verse dayNumber
  -- Basic validation for dayNumber, though problem constraints usually ensure 1-12.
  | dayNumber < 1 || dayNumber > 12 = error "Day number must be between 1 and 12."
  | otherwise =
      let
        ordinal = dayOrdinals !! (dayNumber - 1)
        intro = "On the " ++ ordinal ++ " day of Christmas my true love gave to me: "

        -- Collect gifts for the current day.
        -- 'take dayNumber allGifts' gets gifts from day 1 up to dayNumber.
        -- 'reverse' orders them as they are sung (newest gift first).
        -- e.g., for day 3: ["three French Hens", "two Turtle Doves", "a Partridge in a Pear Tree"]
        currentGiftsForVerse = reverse (take dayNumber allGifts)

        presentsStr = formatPresents currentGiftsForVerse
      in
        intro ++ presentsStr

-- Generates the verses of "The Twelve Days of Christmas" from a start day to an end day.
recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
