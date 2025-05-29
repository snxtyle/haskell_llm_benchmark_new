module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]
  where
    days = words "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"
    gifts = [
        "a Partridge in a Pear Tree",
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

    verse :: Int -> String
    verse day =
        let dayStr = days !! (day-1)
            giftsList = reverse $ take day gifts
        in "On the " ++ dayStr ++ " day of Christmas my true love gave to me: " ++ formatGifts giftsList ++ "."

    formatGifts :: [String] -> String
    formatGifts [] = ""
    formatGifts [x] = x
    formatGifts [x,y] = x ++ ", and " ++ y
    formatGifts (x:xs) = x ++ ", " ++ formatGifts xs
