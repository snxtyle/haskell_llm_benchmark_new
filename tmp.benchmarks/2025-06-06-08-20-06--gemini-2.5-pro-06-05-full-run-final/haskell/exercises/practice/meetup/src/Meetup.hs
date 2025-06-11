module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import qualified Data.Time.Calendar.WeekDate as WD

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

-- Helper function to convert our Weekday to Data.Time's DayOfWeek.
-- This is needed because the constructor names would clash otherwise.
toDayOfWeek :: Weekday -> WD.DayOfWeek
toDayOfWeek Monday    = WD.Monday
toDayOfWeek Tuesday   = WD.Tuesday
toDayOfWeek Wednesday = WD.Wednesday
toDayOfWeek Thursday  = WD.Thursday
toDayOfWeek Friday    = WD.Friday
toDayOfWeek Saturday  = WD.Saturday
toDayOfWeek Sunday    = WD.Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    let allDaysInMonth = [fromGregorian year month d | d <- [1..gregorianMonthLength year month]]
        targetDayOfWeek = toDayOfWeek weekday
        matchingDays = filter (\d -> WD.dayOfWeek d == targetDayOfWeek) allDaysInMonth
    in
    case schedule of
        First  -> matchingDays !! 0
        Second -> matchingDays !! 1
        Third  -> matchingDays !! 2
        Fourth -> matchingDays !! 3
        Last   -> last matchingDays
        Teenth -> head $ filter isTeenth matchingDays
  where
    isTeenth day = let (_, _, dayOfMonth) = toGregorian day in dayOfMonth >= 13 && dayOfMonth <= 19
