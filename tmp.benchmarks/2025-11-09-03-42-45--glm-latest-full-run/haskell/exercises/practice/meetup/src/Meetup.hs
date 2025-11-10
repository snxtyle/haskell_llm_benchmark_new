module Meetup (Weekday(..), Schedule(..), meetupDay) where

import qualified Data.Time.Calendar as Time

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Eq, Show)

-- | Converts our custom Weekday to the standard library's DayOfWeek.
toTimeWeekday :: Weekday -> Time.DayOfWeek
toTimeWeekday Monday    = Time.Monday
toTimeWeekday Tuesday   = Time.Tuesday
toTimeWeekday Wednesday = Time.Wednesday
toTimeWeekday Thursday  = Time.Thursday
toTimeWeekday Friday    = Time.Friday
toTimeWeekday Saturday  = Time.Saturday
toTimeWeekday Sunday    = Time.Sunday

-- | Calculates the date of a meetup.
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Time.Day
meetupDay schedule weekday year month =
    let targetDOW = toTimeWeekday weekday
        daysInMonth = Time.gregorianMonthLength year month
        dayRange = case schedule of
                     First  -> [1..7]
                     Second -> [8..14]
                     Third  -> [15..21]
                     Fourth -> [22..28]
                     Teenth -> [13..19]
                     Last   -> [(daysInMonth - 6)..daysInMonth]
        -- Find the first day in the range that matches the target weekday.
        -- The problem guarantees a match, so head is safe.
        matchingDays = [Time.fromGregorian year month d | d <- dayRange, Time.dayOfWeek (Time.fromGregorian year month d) == targetDOW]
    in head matchingDays
