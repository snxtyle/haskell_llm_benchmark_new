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

-- Convert our Weekday to the Day of Week format used by Data.Time (1-7, Monday-Sunday)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

-- Get the day of week for a given date (1-7, Monday-Sunday)
dayOfWeek :: Day -> Int
dayOfWeek day = let (_, _, dow) = toWeekDate day in dow

-- Find the first day of a month
firstDayOfMonth :: Integer -> Int -> Day
firstDayOfMonth year month = fromGregorian year month 1

-- Find the last day of a month
lastDayOfMonth :: Integer -> Int -> Day
lastDayOfMonth year month = 
    let nextMonth = if month == 12 then 1 else month + 1
        nextYear = if month == 12 then year + 1 else year
        firstOfNextMonth = fromGregorian nextYear nextMonth 1
    in addDays (-1) firstOfNextMonth

-- Find the first occurrence of a weekday on or after a given date
firstOccurrenceOnOrAfter :: Weekday -> Day -> Day
firstOccurrenceOnOrAfter targetWeekday startDate =
    let targetDow = weekdayToInt targetWeekday
        currentDow = dayOfWeek startDate
        diff = (targetDow - currentDow + 7) `mod` 7
    in addDays (fromIntegral diff) startDate

-- Find the first occurrence of a weekday on or before a given date
firstOccurrenceOnOrBefore :: Weekday -> Day -> Day
firstOccurrenceOnOrBefore targetWeekday startDate =
    let targetDow = weekdayToInt targetWeekday
        currentDow = dayOfWeek startDate
        diff = (currentDow - targetDow + 7) `mod` 7
    in addDays (fromIntegral (-diff)) startDate

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay First weekday year month =
    firstOccurrenceOnOrAfter weekday (firstDayOfMonth year month)
meetupDay Second weekday year month =
    let first = meetupDay First weekday year month
    in addDays 7 first
meetupDay Third weekday year month =
    let second = meetupDay Second weekday year month
    in addDays 7 second
meetupDay Fourth weekday year month =
    let third = meetupDay Third weekday year month
    in addDays 7 third
meetupDay Last weekday year month =
    firstOccurrenceOnOrBefore weekday (lastDayOfMonth year month)
meetupDay Teenth weekday year month =
    let teenthStart = fromGregorian year month 13
    in firstOccurrenceOnOrAfter weekday teenthStart
