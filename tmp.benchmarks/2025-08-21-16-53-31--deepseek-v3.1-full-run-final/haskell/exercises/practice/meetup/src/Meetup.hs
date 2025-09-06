module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays, dayOfWeek)
import Data.Time.Calendar.WeekDate (dayOfWeek)

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

-- Convert our Weekday type to the DayOfWeek type from time package
toSystemWeekday :: Weekday -> Int
toSystemWeekday Monday    = 1
toSystemWeekday Tuesday   = 2
toSystemWeekday Wednesday = 3
toSystemWeekday Thursday  = 4
toSystemWeekday Friday    = 5
toSystemWeekday Saturday  = 6
toSystemWeekday Sunday    = 7

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    case schedule of
        Teenth -> findTeenthDay weekday year month
        Last   -> findLastDay weekday year month
        _      -> findNthDay (scheduleToInt schedule) weekday year month
  where
    scheduleToInt :: Schedule -> Int
    scheduleToInt First  = 1
    scheduleToInt Second = 2
    scheduleToInt Third  = 3
    scheduleToInt Fourth = 4
    scheduleToInt _      = error "Invalid schedule for nth day"

    -- Find the nth occurrence of a weekday in the month
    findNthDay :: Int -> Weekday -> Integer -> Int -> Day
    findNthDay n targetWeekday year month =
        let firstDay = fromGregorian year month 1
            targetWday = toSystemWeekday targetWeekday
            firstDayWday = fromEnum (dayOfWeek firstDay) + 1  -- Convert to 1-7 range
            
            -- Calculate days to add to reach the first occurrence of target weekday
            daysToFirst = (targetWday - firstDayWday + 7) `mod` 7
            firstOccurrence = addDays (fromIntegral daysToFirst) firstDay
            
            -- Add weeks to get to the nth occurrence
            nthOccurrence = addDays (fromIntegral ((n - 1) * 7)) firstOccurrence
        in
            -- Check if we're still in the same month
            let (y, m, d) = toGregorian nthOccurrence
            in if m == month then nthOccurrence
               else error "Invalid date - nth occurrence doesn't exist in this month"

    -- Find the teenth day (13-19) for the given weekday
    findTeenthDay :: Weekday -> Integer -> Int -> Day
    findTeenthDay targetWeekday year month =
        let targetWday = toSystemWeekday targetWeekday
            -- Check each day from 13 to 19
            checkDay day =
                let date = fromGregorian year month day
                    wday = fromEnum (dayOfWeek date) + 1  -- Convert to 1-7 range
                in wday == targetWday
            teenthDays = [13..19]
        in case filter checkDay teenthDays of
            [day] -> fromGregorian year month day
            _     -> error "No teenth day found (should never happen)"

    -- Find the last occurrence of a weekday in the month
    findLastDay :: Weekday -> Integer -> Int -> Day
    findLastDay targetWeekday year month =
        let targetWday = toSystemWeekday targetWeekday
            -- Start from the last day of the month
            lastDayOfMonth = if month == 12
                      then fromGregorian (year + 1) 1 1
                      else fromGregorian year (month + 1) 1
            lastDay = addDays (-1) lastDayOfMonth
            
            lastDayWday = fromEnum (dayOfWeek lastDay) + 1  -- Convert to 1-7 range
            
            -- Calculate days to subtract to reach the target weekday
            daysToSubtract = (lastDayWday - targetWday + 7) `mod` 7
        in
            addDays (fromIntegral (-daysToSubtract)) lastDay
