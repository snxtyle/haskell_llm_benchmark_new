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
ordinal _ = error "Invalid day"

gifts :: Int -> String
gifts 1 = "a Partridge in a Pear Tree"
gifts day = giftList day ++ ", and a Partridge in a Pear Tree"

giftList :: Int -> String
giftList day = intercalate ", " (reverse (drop (12 - day) giftItems))

giftItems :: [String]
giftItems = [ "twelve Drummers Drumming"
            , "eleven Pipers Piping"
            , "ten Lords-a-Leaping"
            , "nine Ladies Dancing"
            , "eight Maids-a-Milking"
            , "seven Swans-a-Swimming"
            , "six Geese-a-Laying"
            , "five Gold Rings"
            , "four Calling Birds"
            , "three French Hens"
            , "two Turtle Doves"
            ]

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
