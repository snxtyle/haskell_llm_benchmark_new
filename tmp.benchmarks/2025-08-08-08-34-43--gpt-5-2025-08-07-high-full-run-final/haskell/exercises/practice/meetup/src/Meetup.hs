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

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  case schedule of
    Teenth -> fromGregorian year month (teenthDay weekday year month)
    First  -> nthDay 1
    Second -> nthDay 2
    Third  -> nthDay 3
    Fourth -> nthDay 4
    Last   -> fromGregorian year month (lastDay weekday year month)
  where
    targetDow :: Int
    targetDow = weekdayToInt weekday

    dayOfWeek :: Int -> Int
    dayOfWeek d = let (_, _, w) = toWeekDate (fromGregorian year month d) in w

    nthDay :: Int -> Day
    nthDay n =
      let dow1 = dayOfWeek 1
          offset = (targetDow - dow1) `mod` 7
          d = 1 + offset + 7 * (n - 1)
       in fromGregorian year month d

    teenthDay :: Weekday -> Integer -> Int -> Int
    teenthDay _ y m =
      head [ d | d <- [13 .. 19], let (_, _, w) = toWeekDate (fromGregorian y m d), w == targetDow ]

    lastDay :: Weekday -> Integer -> Int -> Int
    lastDay _ y m =
      let end = gregorianMonthLength y m
          dowEnd = dayOfWeek end
          back = (dowEnd - targetDow) `mod` 7
       in end - back

weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7
