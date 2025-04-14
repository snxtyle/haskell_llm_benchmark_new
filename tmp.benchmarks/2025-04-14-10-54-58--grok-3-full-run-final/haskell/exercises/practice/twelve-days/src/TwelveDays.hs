module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map (verse . show) [start .. stop]

verse :: String -> String
verse dayNum = concat ["On the ", ordinal dayNum, " day of Christmas my true love gave to me: ", gifts (read dayNum :: Int), "."]

ordinal :: String -> String
ordinal n = case n of
    "1" -> "first"
    "2" -> "second"
    "3" -> "third"
    "4" -> "fourth"
    "5" -> "fifth"
    "6" -> "sixth"
    "7" -> "seventh"
    "8" -> "eighth"
    "9" -> "ninth"
    "10" -> "tenth"
    "11" -> "eleventh"
    "12" -> "twelfth"
    _ -> ""

gifts :: Int -> String
gifts day = intercalate ", " (map gift [day, day-1 .. 1])

gift :: Int -> String
gift n = case n of
    1 -> "a Partridge in a Pear Tree"
    2 -> "two Turtle Doves"
    3 -> "three French Hens"
    4 -> "four Calling Birds"
    5 -> "five Gold Rings"
    6 -> "six Geese-a-Laying"
    7 -> "seven Swans-a-Swimming"
    8 -> "eight Maids-a-Milking"
    9 -> "nine Ladies Dancing"
    10 -> "ten Lords-a-Leaping"
    11 -> "eleven Pipers Piping"
    12 -> "twelve Drummers Drumming"
    _ -> ""

intercalate :: String -> [String] -> String
intercalate sep xs = concat (intersperse sep xs)

intersperse :: String -> [String] -> [String]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs
