module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    case schedule of
        Teenth -> findTeenthDay weekday year month
        First -> findNthWeekday 1 weekday year month
        Second -> findNthWeekday 2 weekday year month
        Third -> findNthWeekday 3 weekday year month
        Fourth -> findNthWeekday 4 weekday year month
        Last -> findLastWeekday weekday year month

-- Convert our Weekday to the ISO weekday number (1=Monday, 7=Sunday)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday = 1
weekdayToInt Tuesday = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday = 4
weekdayToInt Friday = 5
weekdayToInt Saturday = 6
weekdayToInt Sunday = 7

-- Find the teenth day (13th-19th) that matches the weekday
findTeenthDay :: Weekday -> Integer -> Int -> Day
findTeenthDay weekday year month = 
    let targetWeekday = weekdayToInt weekday
        teenthDays = [13..19]
        matchingDay = head [day | day <- teenthDays, 
                           let date = fromGregorian year month day
                               (_, _, dayOfWeek) = toWeekDate date
                           in dayOfWeek == targetWeekday]
    in fromGregorian year month matchingDay

-- Find the nth occurrence of a weekday in a month
findNthWeekday :: Int -> Weekday -> Integer -> Int -> Day
findNthWeekday n weekday year month = 
    let targetWeekday = weekdayToInt weekday
        firstDay = fromGregorian year month 1
        (_, _, firstDayOfWeek) = toWeekDate firstDay
        -- Calculate days until first occurrence of target weekday
        daysToFirst = (targetWeekday - firstDayOfWeek) `mod` 7
        firstOccurrence = addDays (fromIntegral daysToFirst) firstDay
        -- Add weeks to get nth occurrence
        nthOccurrence = addDays (fromIntegral ((n - 1) * 7)) firstOccurrence
    in nthOccurrence

-- Find the last occurrence of a weekday in a month
findLastWeekday :: Weekday -> Integer -> Int -> Day
findLastWeekday weekday year month = 
    let targetWeekday = weekdayToInt weekday
        -- Get the last day of the month
        lastDay = fromGregorian year month 31
        (lastYear, lastMonth, lastDayNum) = toGregorian lastDay
        -- If we went into next month, back up to actual last day
        actualLastDay = if lastMonth == month then lastDay 
                       else fromGregorian year month (lastDayNum - 1)
        (_, _, lastDayOfWeek) = toWeekDate actualLastDay
        -- Calculate days to go back to find last occurrence of target weekday
        daysBack = (lastDayOfWeek - targetWeekday) `mod` 7
        lastOccurrence = addDays (negate (fromIntegral daysBack)) actualLastDay
    in lastOccurrence
