module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, dayOfWeek)
-- We need to refer to the constructors of Data.Time.Calendar.DayOfWeek,
-- e.g., Data.Time.Calendar.Monday. These are available through Data.Time.Calendar.
import qualified Data.Time.Calendar as Cal -- Using qualified import for DayOfWeek constructors

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show) -- Added Eq and Show for utility

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Eq, Show) -- Added Eq and Show for utility

-- Helper function to convert our Weekday ADT to Data.Time.Calendar.DayOfWeek
weekdayToCalDayOfWeek :: Weekday -> Cal.DayOfWeek
weekdayToCalDayOfWeek Monday    = Cal.Monday
weekdayToCalDayOfWeek Tuesday   = Cal.Tuesday
weekdayToCalDayOfWeek Wednesday = Cal.Wednesday
weekdayToCalDayOfWeek Thursday  = Cal.Thursday
weekdayToCalDayOfWeek Friday    = Cal.Friday
weekdayToCalDayOfWeek Saturday  = Cal.Saturday
weekdayToCalDayOfWeek Sunday    = Cal.Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    let targetCalDayOfWeek = weekdayToCalDayOfWeek weekday

        -- Generate all days in the specified month and year
        daysInMonth = [fromGregorian year month d | d <- [1 .. gregorianMonthLength year month]]

        -- Filter these days to get only those matching the target weekday
        daysMatchingWeekday = filter (\day -> dayOfWeek day == targetCalDayOfWeek) daysInMonth

    in case schedule of
        First  -> daysMatchingWeekday !! 0
        Second -> daysMatchingWeekday !! 1
        Third  -> daysMatchingWeekday !! 2
        Fourth -> daysMatchingWeekday !! 3
        Last   -> last daysMatchingWeekday -- `last` is safe as list of matching weekdays will not be empty
                                          -- (each weekday occurs at least 4 times in a month)
        Teenth ->
            -- For Teenth, we specifically look at days 13 through 19 of the month
            let teenthCandidateDays = [fromGregorian year month d | d <- [13 .. 19]]
                -- Filter these teenth days to find the one matching the target weekday
                -- The problem guarantees there is exactly one such day.
                teenthDay = head $ filter (\day -> dayOfWeek day == targetCalDayOfWeek) teenthCandidateDays
            in teenthDay
