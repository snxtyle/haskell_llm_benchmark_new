module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = [verse n | n <- [start..stop]]

verse :: Int -> String
verse n = "On the " ++ ordinal n ++ " day of Christmas my true love gave to me: " ++ gifts n ++ "."

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
ordinal _ = error "Invalid day"

gifts :: Int -> String
gifts n = joinGifts $ reverse [gift i n | i <- [1..n]]

joinGifts :: [String] -> String
joinGifts [] = ""
joinGifts [x] = x
joinGifts (x:xs) = x ++ ", " ++ joinGifts xs

gift :: Int -> Int -> String
gift 1 1 = "a Partridge in a Pear Tree"
gift 1 _ = "and a Partridge in a Pear Tree"
gift 2 _ = "two Turtle Doves"
gift 3 _ = "three French Hens"
gift 4 _ = "four Calling Birds"
gift 5 _ = "five Gold Rings"
gift 6 _ = "six Geese-a-Laying"
gift 7 _ = "seven Swans-a-Swimming"
gift 8 _ = "eight Maids-a-Milking"
gift 9 _ = "nine Ladies Dancing"
gift 10 _ = "ten Lords-a-Leaping"
gift 11 _ = "eleven Pipers Piping"
gift 12 _ = "twelve Drummers Drumming"
gift _ _ = error "Invalid gift number"
