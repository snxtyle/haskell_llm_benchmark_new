module Meetup (Weekday(..), Schedule(..), meetupDay) where

import qualified Data.Time.Calendar as C

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

toStdDayOfWeek :: Weekday -> C.DayOfWeek
toStdDayOfWeek Monday    = C.Monday
toStdDayOfWeek Tuesday   = C.Tuesday
toStdDayOfWeek Wednesday = C.Wednesday
toStdDayOfWeek Thursday  = C.Thursday
toStdDayOfWeek Friday    = C.Friday
toStdDayOfWeek Saturday  = C.Saturday
toStdDayOfWeek Sunday    = C.Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> C.Day
meetupDay schedule weekday year month = 
  let firstDay = C.fromGregorian year month 1
      lastDayNum = C.gregorianMonthLength year month
      lastDay = C.fromGregorian year month lastDayNum
      allDays = [firstDay .. lastDay]
      stdW = toStdDayOfWeek weekday
      wDays = filter (\d -> C.dayOfWeek d == stdW) allDays
  in case schedule of
       First  -> wDays !! 0
       Second -> wDays !! 1
       Third  -> wDays !! 2
       Fourth -> wDays !! 3
       Last   -> last wDays
       Teenth -> head $ filter (\d -> let (_,_,day) = C.toGregorian d in day >= 13 && day <= 19) wDays
