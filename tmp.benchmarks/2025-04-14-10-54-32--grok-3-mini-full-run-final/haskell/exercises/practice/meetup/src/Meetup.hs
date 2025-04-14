module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Maybe (isJust)
import Data.Time.Calendar (Day, fromGregorian, fromGregorianValid)
import Data.Time.Format (formatTime, defaultTimeLocale)

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

stringToWeekday :: String -> Weekday
stringToWeekday s = case s of
  "Monday"    -> Monday
  "Tuesday"   -> Tuesday
  "Wednesday" -> Wednesday
  "Thursday"  -> Thursday
  "Friday"    -> Friday
  "Saturday"  -> Saturday
  "Sunday"    -> Sunday
  _           -> error "Invalid weekday string"

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
  let daysInMonth = [d | d <- [1..31], isJust (fromGregorianValid year month d)]
      matchingDays = [d | d <- daysInMonth, stringToWeekday (formatTime defaultTimeLocale "%A" (fromGregorian year month d)) == weekday]
  in case schedule of
       First  -> fromGregorian year month (matchingDays !! 0)
       Second -> fromGregorian year month (matchingDays !! 1)
       Third  -> fromGregorian year month (matchingDays !! 2)
       Fourth -> fromGregorian year month (matchingDays !! 3)
       Last   -> fromGregorian year month (last matchingDays)
       Teenth -> fromGregorian year month (head [d | d <- matchingDays, d >= 13 && d <= 19])
