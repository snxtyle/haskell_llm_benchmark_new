module LeapYear (isLeapYear) where

-- | Determine whether a given year is a leap year in the Gregorian calendar.
--
-- A leap year is:
--   • Every year that is evenly divisible by 4,
--   • Except every year that is evenly divisible by 100,
--   • Unless the year is also evenly divisible by 400.
--
-- Examples:
--
-- >>> isLeapYear 1997
-- False
-- >>> isLeapYear 1900
-- False
-- >>> isLeapYear 2000
-- True
isLeapYear :: Integer -> Bool
isLeapYear year =
  divisibleBy 4 year &&
  (not (divisibleBy 100 year) || divisibleBy 400 year)
  where
    divisibleBy :: Integer -> Integer -> Bool
    divisibleBy n x = x `mod` n == 0
