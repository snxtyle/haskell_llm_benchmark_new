module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Enum)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    case schedule of
        First  -> occurrences !! 0
        Second -> occurrences !! 1
        Third  -> occurrences !! 2
        Fourth -> occurrences !! 3
        Last   -> last occurrences
        Teenth -> head $ filter isTeenth occurrences
  where
    -- Get all occurrences of the weekday in the month
    occurrences = filter (isWeekday weekday) $ daysInMonth year month
    
    -- Check if a day is in the teenth range (13-19)
    isTeenth day = let (_, _, d) = toGregorian day in d >= 13 && d <= 19

-- Get all days in a given month
daysInMonth :: Integer -> Int -> [Day]
daysInMonth year month = 
    [fromGregorian year month d | d <- [1..lastDay]]
  where
    lastDay = case month of
        2  -> if isLeapYear year then 29 else 28
        4  -> 30
        6  -> 30
        9  -> 30
        11 -> 30
        _  -> 31

-- Check if a year is a leap year
isLeapYear :: Integer -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- Check if a Day is a specific Weekday
isWeekday :: Weekday -> Day -> Bool
isWeekday targetWeekday day = 
    dayOfWeek day == weekdayToInt targetWeekday
  where
    -- Convert our Weekday type to ISO day of week (1=Monday, 7=Sunday)
    weekdayToInt Monday    = 1
    weekdayToInt Tuesday   = 2
    weekdayToInt Wednesday = 3
    weekdayToInt Thursday  = 4
    weekdayToInt Friday    = 5
    weekdayToInt Saturday  = 6
    weekdayToInt Sunday    = 7

-- Get the day of week for a Day (1=Monday, 7=Sunday)
dayOfWeek :: Day -> Int
dayOfWeek day = let (_, _, dow) = toWeekDate day in dow
