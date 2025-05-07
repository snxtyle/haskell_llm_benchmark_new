module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
  where
    days = ["first", "second", "third", "fourth", "fifth", "sixth",
            "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

    gifts = [ "a Partridge in a Pear Tree."
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

    verse n = "On the " ++ days !! (n-1) ++ " day of Christmas my true love gave to me: " ++ giftsLine n

    giftsLine 1 = gifts !! 0
    giftsLine n = concatMap giftPart [n-1,n-2..0]

    giftPart 0 = "and " ++ gifts !! 0
    giftPart i = gifts !! i ++ ", "
