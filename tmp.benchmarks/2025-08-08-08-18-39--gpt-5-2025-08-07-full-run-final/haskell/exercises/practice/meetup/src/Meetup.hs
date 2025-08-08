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

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  fromGregorian year month dayOfMonth
  where
    targetDow :: Int
    targetDow = weekdayToInt weekday

    firstDay :: Day
    firstDay = fromGregorian year month 1

    firstDow :: Int
    firstDow = dow firstDay

    firstOccurrenceDom :: Int
    firstOccurrenceDom = 1 + ((targetDow - firstDow) `mod` 7)

    monthLen :: Int
    monthLen = gregorianMonthLength year month

    dayOfMonth :: Int
    dayOfMonth =
      case schedule of
        First  -> firstOccurrenceDom
        Second -> firstOccurrenceDom + 7
        Third  -> firstOccurrenceDom + 14
        Fourth -> firstOccurrenceDom + 21
        Teenth ->
          let dow13 = dow (fromGregorian year month 13)
              delta = (targetDow - dow13) `mod` 7
           in 13 + delta
        Last   ->
          let dowLast = dow (fromGregorian year month monthLen)
              deltaBack = (dowLast - targetDow) `mod` 7
           in monthLen - deltaBack

weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

dow :: Day -> Int
dow day =
  let (_, _, d) = toWeekDate day
  in d
