module LeapYear (isLeapYear) where

-- | Determine whether a given year is a leap year in the Gregorian calendar.
--   A year is a leap year if it is divisible by 4,
--   except years that are divisible by 100,
--   unless they are also divisible by 400.
isLeapYear :: Integer -> Bool
isLeapYear year =
  year `mod` 4 == 0
    && (year `mod` 100 /= 0 || year `mod` 400 == 0)
