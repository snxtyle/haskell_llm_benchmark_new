module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]

verse :: Int -> String
verse day = "On the " ++ ordinal day ++ " day of Christmas my true love gave to me: " ++ gifts day ++ "."

ordinal :: Int -> String
ordinal 1 = "first"
ordinal 2 = "second"
ordinal 3 = "third"
ordinal 4 = "fourth"
ordinal 5 = "fifth"
ordinal 6 = "sixth"
ordinal 7 = "seventh"
ordinal 8 = "eighth"
ordinal 9 = "ninth"
ordinal 10 = "tenth"
ordinal 11 = "eleventh"
ordinal 12 = "twelfth"
ordinal _ = "unknown"

gifts :: Int -> String
gifts 1 = "a Partridge in a Pear Tree"
gifts n = gift n ++ ", " ++ if n == 2 then "and " else "" ++ gifts (n - 1)

gift :: Int -> String
gift 1 = "a Partridge in a Pear Tree"
gift 2 = "two Turtle Doves"
gift 3 = "three French Hens"
gift 4 = "four Calling Birds"
gift 5 = "five Gold Rings"
gift 6 = "six Geese-a-Laying"
gift 7 = "seven Swans-a-Swimming"
gift 8 = "eight Maids-a-Milking"
gift 9 = "nine Ladies Dancing"
gift 10 = "ten Lords-a-Leaping"
gift 11 = "eleven Pipers Piping"
gift 12 = "twelve Drummers Drumming"
gift _ = "unknown gift"
