module Meetup (Weekday(..), Schedule(..), meetupDay) where

-- Import necessary functions and types from Data.Time.Calendar
import Data.Time.Calendar (Day, DayOfWeek(..), dayOfWeek, fromGregorian, gregorianMonthLength, toGregorian)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show) -- Add Eq for comparison

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Eq, Show)

-- Helper function to map our Weekday enum to the DayOfWeek enum from Data.Time
toDayOfWeek :: Weekday -> DayOfWeek
toDayOfWeek Meetup.Monday    = Monday
toDayOfWeek Meetup.Tuesday   = Tuesday
toDayOfWeek Meetup.Wednesday = Wednesday
toDayOfWeek Meetup.Thursday  = Thursday
toDayOfWeek Meetup.Friday    = Friday
toDayOfWeek Meetup.Saturday  = Saturday
toDayOfWeek Meetup.Sunday    = Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    let
        -- Convert the input Weekday to the DayOfWeek type used by the time library
        targetDayOfWeek = toDayOfWeek weekday

        -- Calculate the number of days in the given month and year
        daysInMonth = gregorianMonthLength year month

        -- Generate a list of all Day values for the given month
        allDaysInMonth = [ fromGregorian year month d | d <- [1..daysInMonth] ]

        -- Filter the days to find only those matching the target DayOfWeek
        matchingWeekdays = filter (\d -> dayOfWeek d == targetDayOfWeek) allDaysInMonth

        -- Helper function to extract the day part (1-31) from a Day
        getDayOfMonth :: Day -> Int
        getDayOfMonth day = let (_, _, d) = toGregorian day in d

        -- Find the specific day based on the Schedule
        findDate :: Day
        findDate = case schedule of
            First  -> matchingWeekdays !! 0 -- Get the first matching day
            Second -> matchingWeekdays !! 1 -- Get the second matching day
            Third  -> matchingWeekdays !! 2 -- Get the third matching day
            Fourth -> matchingWeekdays !! 3 -- Get the fourth matching day
            Last   -> last matchingWeekdays -- Get the last matching day
            Teenth -> -- Find the matching day that falls between the 13th and 19th
                      head $ filter (\d -> let dayNum = getDayOfMonth d in dayNum >= 13 && dayNum <= 19) matchingWeekdays

    in findDate -- Return the calculated date
