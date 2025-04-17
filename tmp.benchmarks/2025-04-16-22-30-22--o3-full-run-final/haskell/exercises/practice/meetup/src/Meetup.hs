module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar           (Day, fromGregorian, toGregorian,
                                     gregorianMonthLength)
import Data.Time.Calendar.WeekDate  (toWeekDate)

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

-- | Convert our Weekday enumeration to the integer representation used by
--   'toWeekDate' (Monday = 1 .. Sunday = 7).
weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

-- | Check whether a 'Day' falls on the requested weekday.
matchesWeekday :: Weekday -> Day -> Bool
matchesWeekday wd day = weekdayToInt wd == w
  where
    (_, _, w) = toWeekDate day   -- w :: Int, Monday = 1 .. Sunday = 7

-- | Check whether a 'Day' is a "teenth" day (13..19 of a month).
isTeenth :: Day -> Bool
isTeenth day = d >= 13 && d <= 19
  where
    (_, _, d) = toGregorian day

-- | Produce all 'Day's for the given month.
daysInMonth :: Integer -> Int -> [Day]
daysInMonth year month =
  [ fromGregorian year month d | d <- [1 .. gregorianMonthLength year month] ]

-- | Retrieve the n‑th (0‑based) element of a list that must contain enough
--   elements.  This keeps the pattern matching in 'meetupDay' succinct and
--   emphasises that the input guarantees a valid result.
nth :: Int -> [a] -> a
nth n = head . drop n

-- | Determine the exact meetup day for the supplied schedule, weekday, year
--   and month.
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  case schedule of
    First  -> nth 0 matchingDays
    Second -> nth 1 matchingDays
    Third  -> nth 2 matchingDays
    Fourth -> nth 3 matchingDays
    Last   -> last matchingDays
    Teenth -> head (filter isTeenth matchingDays)
  where
    matchingDays = filter (matchesWeekday weekday) (daysInMonth year month)
