module Phone (number) where

import Control.Monad (guard)
import Data.Char (isDigit)

number :: String -> Maybe String
number xs = do
  let digits = filter isDigit xs
  tenDigits <- case length digits of
    10 -> Just digits
    11 -> if head digits == '1' then Just (tail digits) else Nothing
    _ -> Nothing

  guard (head tenDigits `elem` ['2' .. '9'])
  guard ((tenDigits !! 3) `elem` ['2' .. '9'])

  return tenDigits
