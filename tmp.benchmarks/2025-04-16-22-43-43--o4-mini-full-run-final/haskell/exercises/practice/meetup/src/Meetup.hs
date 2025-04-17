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

-- | Given a schedule (e.g. First, Second, Teenth, Last), a weekday,
--   a year and a month, returns the Day of the meetup.
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
  let
    -- length of the month
    monthLen = gregorianMonthLength year month

    -- map our Weekday to ISO weekday number (1=Monday .. 7=Sunday)
    weekdayNum :: Weekday -> Int
    weekdayNum wd = case wd of
      Monday    -> 1
      Tuesday   -> 2
      Wednesday -> 3
      Thursday  -> 4
      Friday    -> 5
      Saturday  -> 6
      Sunday    -> 7

    targetW = weekdayNum weekday

    -- all days in the month matching the requested weekday
    matchingDays :: [Int]
    matchingDays =
      [ d
      | d <- [1 .. monthLen]
      , let day = fromGregorian year month d
            (_, _, w) = toWeekDate day
      , w == targetW
      ]

    -- pick the correct day number based on the schedule
    chosenDay :: Int
    chosenDay = case schedule of
      First   -> matchingDays !! 0
      Second  -> matchingDays !! 1
      Third   -> matchingDays !! 2
      Fourth  -> matchingDays !! 3
      Last    -> last matchingDays
      Teenth  -> head $ filter (\d -> d >= 13 && d <= 19) matchingDays

  in fromGregorian year month chosenDay
