module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays, gregorianMonthLength, toGregorian)
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

-- Map our Weekday to ISO week day number (Monday=1 .. Sunday=7)
weekdayToIso :: Weekday -> Int
weekdayToIso Monday    = 1
weekdayToIso Tuesday   = 2
weekdayToIso Wednesday = 3
weekdayToIso Thursday  = 4
weekdayToIso Friday    = 5
weekdayToIso Saturday  = 6
weekdayToIso Sunday    = 7

-- Get ISO weekday of a Day
dayIsoWeekday :: Day -> Int
dayIsoWeekday day = let (_, _, w) = toWeekDate day in w

-- Find the first occurrence of the given weekday on or after the given day
firstOnOrAfter :: Weekday -> Day -> Day
firstOnOrAfter w d =
  let target = weekdayToIso w
      cur    = dayIsoWeekday d
      delta  = (target - cur) `mod` 7
  in addDays (fromIntegral delta) d

-- Find the last occurrence of the given weekday on or before the given day
lastOnOrBefore :: Weekday -> Day -> Day
lastOnOrBefore w d =
  let target = weekdayToIso w
      cur    = dayIsoWeekday d
      delta  = (cur - target) `mod` 7
  in addDays (negate (fromIntegral delta)) d

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  case schedule of
    First  -> nth 0
    Second -> nth 1
    Third  -> nth 2
    Fourth -> nth 3
    Last   -> lastInMonth
    Teenth -> teenth
  where
    firstOfMonth :: Day
    firstOfMonth = fromGregorian year month 1

    nth :: Int -> Day
    nth n =
      let firstWanted = firstOnOrAfter weekday firstOfMonth
      in addDays (fromIntegral (n * 7)) firstWanted

    lastInMonth :: Day
    lastInMonth =
      let lastDayNum = gregorianMonthLength year month
          lastOfMonth = fromGregorian year month (fromIntegral lastDayNum)
      in lastOnOrBefore weekday lastOfMonth

    teenth :: Day
    teenth =
      let startTeenth = fromGregorian year month 13
      in firstOnOrAfter weekday startTeenth
