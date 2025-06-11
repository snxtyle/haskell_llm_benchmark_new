module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

-- | Our internal representation of weekdays.
--   Note: the numbering matches the ISO-8601 / `toWeekDate` convention
--   where Monday = 1 … Sunday = 7.
data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)

-- | Which occurrence of the weekday we are interested in.
data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Eq, Show)

-- | Calculate the date of the meetup according to the given schedule.
--
--   @meetupDay schedule weekday year month@
--       * 'schedule' – occurrence specifier (first, second, …, teenth)
--       * 'weekday'  – day of the week we are looking for
--       * 'year'     – four‐digit year (e.g. 2024)
--       * 'month'    – month number (1‥12)
--
--   The function always returns a valid 'Day'.  If the requested
--   occurrence does not exist (e.g. "fifth Monday" in February),
--   the function raises an error.
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  let daysInMonth = gregorianMonthLength year month
      -- Construct all days of the month.
      allDays :: [(Int, Day)]
      allDays = [ (d, fromGregorian year month d) | d <- [1 .. daysInMonth] ]

      -- Map our Weekday value to ISO-8601 int (Monday = 1 … Sunday = 7)
      wantedDow :: Int
      wantedDow = case weekday of
                    Monday    -> 1
                    Tuesday   -> 2
                    Wednesday -> 3
                    Thursday  -> 4
                    Friday    -> 5
                    Saturday  -> 6
                    Sunday    -> 7

      -- Select only the days that match the requested weekday
      matching :: [(Int, Day)]
      matching = filter (\(_, day) -> let (_, _, dow) = toWeekDate day
                                      in dow == wantedDow) allDays

      pick :: [a] -> Int -> a
      pick xs idx =
        case drop idx xs of
          (y:_) -> y
          []    -> error "Requested meetup day does not exist in this month."

      chosenDay :: Day
      chosenDay =
        case schedule of
          First  -> snd $ pick matching 0
          Second -> snd $ pick matching 1
          Third  -> snd $ pick matching 2
          Fourth -> snd $ pick matching 3
          Last   -> snd $ last matching
          Teenth ->
            let inTeens = filter (\(d, _) -> d >= 13 && d <= 19) matching
            in case inTeens of
                 [(_, day)] -> day
                 []         -> error "No teenth day found for the given weekday."
                 _          -> error "Invariant violated: more than one teenth day."
  in chosenDay
