module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, dayOfWeek)
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

-- Convert Weekday to numeric representation (1 = Monday, 7 = Sunday)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday = 1
weekdayToInt Tuesday = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday = 4
weekdayToInt Friday = 5
weekdayToInt Saturday = 6
weekdayToInt Sunday = 7

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = findDay schedule (weekdayToInt weekday) year month
  where
    findDay :: Schedule -> Int -> Integer -> Int -> Day
    findDay First targetWeekday y m = findNthWeekday 1 targetWeekday y m
    findDay Second targetWeekday y m = findNthWeekday 2 targetWeekday y m
    findDay Third targetWeekday y m = findNthWeekday 3 targetWeekday y m
    findDay Fourth targetWeekday y m = findNthWeekday 4 targetWeekday y m
    findDay Teenth targetWeekday y m = head [fromGregorian y m d | d <- [13..19], getWeekday y m d == targetWeekday]
    findDay Last targetWeekday y m = 
        let daysInMonth = gregorianMonthLength y m
            candidates = [fromGregorian y m d | d <- [1..daysInMonth], getWeekday y m d == targetWeekday]
        in last candidates

    -- Get the weekday (1-7) for a specific date
    getWeekday :: Integer -> Int -> Int -> Int
    getWeekday y m d = (\(_, _, wd) -> wd) $ toWeekDate $ fromGregorian y m d
    
    -- Find the nth occurrence of a weekday in a month
    findNthWeekday :: Int -> Int -> Integer -> Int -> Day
    findNthWeekday n targetWeekday y m = candidates !! (n - 1)
      where
        candidates = [fromGregorian y m d | d <- [1..(gregorianMonthLength y m)], getWeekday y m d == targetWeekday]
