module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

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

-- Convert our Weekday to ISO 8601 weekday number (Monday=1 .. Sunday=7)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

-- Get the weekday of a given Day as ISO 8601 weekday number
dayWeekday :: Day -> Int
dayWeekday day = let (_, _, w) = toWeekDate day in w

-- Find all days in the month that match the given weekday
daysInMonthByWeekday :: Integer -> Int -> Int -> [Int]
daysInMonthByWeekday year month targetWeekday =
  let monthLength = gregorianMonthLength year month
      days = [1..monthLength]
      matches = filter (\d -> dayWeekday (fromGregorian year month d) == targetWeekday) days
  in matches

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  let targetWeekday = weekdayToInt weekday
      matchingDays = daysInMonthByWeekday year month targetWeekday
  in case schedule of
       First  -> fromGregorian year month (matchingDays !! 0)
       Second -> fromGregorian year month (matchingDays !! 1)
       Third  -> fromGregorian year month (matchingDays !! 2)
       Fourth -> fromGregorian year month (matchingDays !! 3)
       Last   -> fromGregorian year month (last matchingDays)
       Teenth ->
         let teenthDays = filter (\d -> d >= 13 && d <= 19) matchingDays
         in fromGregorian year month (head teenthDays)
