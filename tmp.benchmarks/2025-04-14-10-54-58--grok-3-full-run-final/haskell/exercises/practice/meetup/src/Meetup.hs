module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    let firstDayOfMonth = fromGregorian year month 1
        daysInMonth = gregorianMonthLength year month
        targetWeekday = fromEnum weekday + 1  -- toWeekDate uses 1-7 for Monday-Sunday
        candidates = filter (\day -> let (_, _, wday) = toWeekDate day in wday == targetWeekday) 
                     [firstDayOfMonth .. addDays (fromIntegral daysInMonth - 1) firstDayOfMonth]
    in case schedule of
        First  -> candidates !! 0
        Second -> candidates !! 1
        Third  -> candidates !! 2
        Fourth -> candidates !! 3
        Last   -> last candidates
        Teenth -> let teenthCandidates = filter (\day -> let (_, _, dayOfMonth) = toGregorian day in dayOfMonth >= 13 && dayOfMonth <= 19) candidates
                  in if null teenthCandidates then error "No matching teenth day found" else head teenthCandidates

-- Helper function to get the number of days in a month
gregorianMonthLength :: Integer -> Int -> Int
gregorianMonthLength year month =
    let day = fromGregorian year month 1
        (_, days) = toOrdinalDate $ addDays 31 day
    in if days > 31 then days - 366 else days - 1
