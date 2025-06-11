module LeapYear (isLeapYear) where

-- | Determine whether a given year is a leap year in the Gregorian calendar.
--   Rules:
--   1. Every year divisible by 400 is a leap year.
--   2. Every year divisible by 100, but not by 400, is NOT a leap year.
--   3. Every year divisible by 4, but not by 100, IS a leap year.
--   4. All other years are NOT leap years.
isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4   == 0 = True
  | otherwise           = False
