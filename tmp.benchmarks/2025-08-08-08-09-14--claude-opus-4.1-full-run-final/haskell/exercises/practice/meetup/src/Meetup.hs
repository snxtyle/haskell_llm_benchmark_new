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
        First  -> daysOfWeekday !! 0
        Second -> daysOfWeekday !! 1
        Third  -> daysOfWeekday !! 2
        Fourth -> daysOfWeekday !! 3
        Last   -> last daysOfWeekday
        Teenth -> head $ filter isTeenth daysOfWeekday
  where
    -- Get all days in the month
    firstDay = fromGregorian year month 1
    lastDayNum = lastDayOfMonth year month
    allDays = [fromGregorian year month d | d <- [1..lastDayNum]]
    
    -- Filter to get only the days matching the requested weekday
    daysOfWeekday = filter (isWeekday weekday) allDays
    
    -- Check if a day is in the teenth range (13-19)
    isTeenth day = 
        let (_, _, d) = toGregorian day
        in d >= 13 && d <= 19

-- Check if a given Day matches the requested Weekday
isWeekday :: Weekday -> Day -> Bool
isWeekday targetWeekday day =
    let (_, _, dayOfWeek) = toWeekDate day
        -- toWeekDate returns 1 for Monday, 7 for Sunday
        -- Our Weekday enum starts at 0 for Monday, 6 for Sunday
    in dayOfWeek == fromEnum targetWeekday + 1

-- Get the last day number of a month
lastDayOfMonth :: Integer -> Int -> Int
lastDayOfMonth year month
    | month `elem` [1,3,5,7,8,10,12] = 31
    | month `elem` [4,6,9,11] = 30
    | month == 2 = if isLeapYear year then 29 else 28
    | otherwise = error "Invalid month"

-- Check if a year is a leap year
isLeapYear :: Integer -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)
