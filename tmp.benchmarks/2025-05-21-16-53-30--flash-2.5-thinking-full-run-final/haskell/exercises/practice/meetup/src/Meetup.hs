module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, dayOfWeek, gregorianMonthLength)
import qualified Data.Time.Calendar.WeekDate as DT -- Qualified import for DayOfWeek constructors

-- My custom Weekday type
data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Eq, Show)

-- My custom Schedule type
data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
  deriving (Eq, Show)

-- Helper to convert our custom Weekday to Data.Time.Calendar.WeekDate.DayOfWeek
weekdayToDayOfWeek :: Weekday -> DT.DayOfWeek
weekdayToDayOfWeek Monday    = DT.Monday
weekdayToDayOfWeek Tuesday   = DT.Tuesday
weekdayToDayOfWeek Wednesday = DT.Wednesday
weekdayToDayOfWeek Thursday  = DT.Thursday
weekdayToDayOfWeek Friday    = DT.Friday
weekdayToDayOfWeek Saturday  = DT.Saturday
weekdayToDayOfWeek Sunday    = DT.Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    let
        targetDayOfWeek = weekdayToDayOfWeek weekday
        daysInMonth = gregorianMonthLength year month

        -- Generate a list of all days in the month
        allDaysInMonth = [fromGregorian year month d | d <- [1 .. daysInMonth]]

        -- Helper to find the Nth occurrence of a weekday in a list of days
        findNthDay :: Int -> [Day] -> Day
        findNthDay n = go 0
            where
                go count (d:ds)
                    | dayOfWeek d == targetDayOfWeek =
                        if count + 1 == n
                            then d
                            else go (count + 1) ds
                    | otherwise = go count ds
                go _ [] = error "Could not find Nth day (this case should not be reached for valid inputs)"
    in
    case schedule of
        First -> findNthDay 1 allDaysInMonth
        Second -> findNthDay 2 allDaysInMonth
        Third -> findNthDay 3 allDaysInMonth
        Fourth -> findNthDay 4 allDaysInMonth
        Last ->
            -- Iterate backwards from the end of the month to find the last occurrence
            let
                reversedDays = reverse allDaysInMonth
            in
            head $ filter (\d -> dayOfWeek d == targetDayOfWeek) reversedDays
        Teenth ->
            -- Check days from 13th to 19th
            let
                teenthDays = [fromGregorian year month d | d <- [13 .. 19]]
            in
            head $ filter (\d -> dayOfWeek d == targetDayOfWeek) teenthDays
