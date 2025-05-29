module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, dayOfWeek, gregorianMonthLength)

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

weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    let monthLen = gregorianMonthLength year month
        candidateDays = [ day | day <- [1..monthLen], 
                          let d = fromGregorian year month day,
                          fromEnum (dayOfWeek d) == timeWeekdayValue ]
    in case schedule of
            First  -> fromGregorian year month (candidateDays !! 0)
            Second -> fromGregorian year month (candidateDays !! 1)
            Third  -> fromGregorian year month (candidateDays !! 2)
            Fourth -> fromGregorian year month (candidateDays !! 3)
            Last   -> fromGregorian year month (last candidateDays)
            Teenth -> let targetDay = head [ d | d <- candidateDays, d >= 13, d <= 19 ] 
                      in fromGregorian year month targetDay
    where
        timeWeekdayValue = weekdayToInt weekday - 1
