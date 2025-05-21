module TwelveDays (recite) where

import Data.List (intercalate)

-- Ordinal numbers for days (1-indexed)
ordinals :: [String]
ordinals = ["first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

-- Gifts for each day (0-indexed)
-- Note: The first gift is "a Partridge in a Pear Tree."
-- The others start with their number.
gifts :: [String]
gifts = ["a Partridge in a Pear Tree", -- Removed the period here
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
         "twelve Drummers Drumming"]

-- Helper function to construct the gifts part of a verse for a given day number (1-indexed)
getGiftsString :: Int -> String
getGiftsString dayNum =
    let
        -- Take the first `dayNum` gifts from the `gifts` list.
        -- Example: day 3 -> ["a Partridge in a Pear Tree.", "two Turtle Doves", "three French Hens"]
        currentDayGifts = take dayNum gifts
        -- Reverse them to get the order for the verse: latest gift, then previous, etc.
        -- Example: day 3 -> ["three French Hens", "two Turtle Doves", "a Partridge in a Pear Tree."]
        reversedGifts = reverse currentDayGifts
    in
        case reversedGifts of
            [] -> "" -- Should not happen for valid dayNum (1 to 12)
            [g] -> g -- For day 1, it's just "a Partridge in a Pear Tree"
            -- For day > 1, the last gift (Partridge) needs "and " prefix.
            -- `g` is the *last* element of the original `currentDayGifts` list,
            -- which is the first gift ("a Partridge in a Pear Tree").
            -- `gs` are the other gifts in reverse order of their day.
            (g:gs) -> intercalate ", " (reverse gs) ++ ", and " ++ g

-- Function to get a single verse for a given day number (1-indexed)
getVerse :: Int -> String
getVerse dayNum =
    let
        -- Get the ordinal string for the day (e.g., "first", "second")
        ordinal = ordinals !! (dayNum - 1)
        -- Get the string representing the gifts for this day
        giftsPart = getGiftsString dayNum
    in
        "On the " ++ ordinal ++ " day of Christmas my true love gave to me: " ++ giftsPart ++ "."

-- The main recite function
recite :: Int -> Int -> [String]
recite start stop = map getVerse [start..stop]
