module TwelveDays (recite) where

ordinals :: [String]
ordinals =
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

gifts :: [String]
gifts =
  [ "a Partridge in a Pear Tree."
  , "two Turtle Doves, "
  , "three French Hens, "
  , "four Calling Birds, "
  , "five Gold Rings, "
  , "six Geese-a-Laying, "
  , "seven Swans-a-Swimming, "
  , "eight Maids-a-Milking, "
  , "nine Ladies Dancing, "
  , "ten Lords-a-Leaping, "
  , "eleven Pipers Piping, "
  , "twelve Drummers Drumming, "
  ]

verse :: Int -> String
verse n =
  "On the " ++ ordinals !! (n - 1) ++ " day of Christmas my true love gave to me: "
  ++ giftsLine
  where
    -- Build the gifts list from n down to 1, inserting "and " before the final gift if n > 1
    giftsLine
      | n == 1 = gifts !! 0
      | otherwise =
          concat (reverse (take (n - 1) (drop 1 gifts)))
          ++ "and "
          ++ gifts !! 0

recite :: Int -> Int -> [String]
recite start stop = map verse [start .. stop]
