module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

-- | Representation of the days of the week.
data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show)

-- | Representation of meetup schedules within a month.
data Schedule
    = First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth
    deriving (Eq, Show)

-- | Convert our Weekday type to the ISO‑8601 day‑of‑week number
--   expected by 'toWeekDate' (Monday = 1 .. Sunday = 7).
weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

-- | Retrieve the ISO‑8601 weekday number for a given 'Day'.
dayOfWeekInt :: Day -> Int
dayOfWeekInt day = let (_, _, w) = toWeekDate day in w

-- | Determine the exact date of the meetup.
meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    case schedule of
        Teenth -> head teenthCandidates
        Last   -> last monthCandidates
        First  -> monthCandidates !! 0
        Second -> monthCandidates !! 1
        Third  -> monthCandidates !! 2
        Fourth -> monthCandidates !! 3
  where
    wInt = weekdayToInt weekday

    -- All days in the specified month matching the requested weekday.
    monthCandidates :: [Day]
    monthCandidates =
        [ d
        | dayNum <- [1 .. gregorianMonthLength year month]
        , let d = fromGregorian year month dayNum
        , dayOfWeekInt d == wInt
        ]

    -- All matching weekdays that fall on a "teenth" day.
    teenthCandidates :: [Day]
    teenthCandidates =
        [ d
        | dayNum <- [13 .. 19]
        , let d = fromGregorian year month dayNum
        , dayOfWeekInt d == wInt
        ]
