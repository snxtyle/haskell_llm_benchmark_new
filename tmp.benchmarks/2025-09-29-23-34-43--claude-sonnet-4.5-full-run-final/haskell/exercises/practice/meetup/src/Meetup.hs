module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, toGregorian)
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
    let daysInMonth = gregorianMonthLength year month
        allDays = [fromGregorian year month d | d <- [1..daysInMonth]]
        matchingDays = filter (isWeekday weekday) allDays
    in case schedule of
        First  -> matchingDays !! 0
        Second -> matchingDays !! 1
        Third  -> matchingDays !! 2
        Fourth -> matchingDays !! 3
        Last   -> last matchingDays
        Teenth -> head $ filter isTeenth matchingDays
  where
    isWeekday :: Weekday -> Day -> Bool
    isWeekday wd day =
        let (_, _, dayOfWeek) = toWeekDate day
        in dayOfWeek == weekdayToInt wd
    
    weekdayToInt :: Weekday -> Int
    weekdayToInt Monday    = 1
    weekdayToInt Tuesday   = 2
    weekdayToInt Wednesday = 3
    weekdayToInt Thursday  = 4
    weekdayToInt Friday    = 5
    weekdayToInt Saturday  = 6
    weekdayToInt Sunday    = 7
    
    isTeenth :: Day -> Bool
    isTeenth day =
        let (_, _, dayNum) = toGregorian day
        in dayNum >= 13 && dayNum <= 19
