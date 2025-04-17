module Phone (number) where

import Data.Char (isDigit)

-- | Clean up a phone number so that it can be sent SMS messages.
--   Returns a 10‑digit number when the input is valid, otherwise Nothing.
--
--   Rules (based on the North American Numbering Plan):
--   * Strip all non‑digit characters.
--   * A valid number has either:
--       • exactly 10 digits, or
--       • 11 digits where the first digit is '1' (country code),
--         in which case the leading '1' is removed.
--   * The first digit of the area code (digits !! 0) must be 2‑9.
--   * The first digit of the exchange code (digits !! 3) must be 2‑9.
number :: String -> Maybe String
number input =
  case cleaned of
    ds | length ds == 10                          -> validate ds
       | length ds == 11 && head ds == '1'        -> validate (tail ds)
       | otherwise                                -> Nothing
  where
    cleaned :: String
    cleaned = filter isDigit input

    -- Validate the structural rules for a 10‑digit NANP number.
    validate :: String -> Maybe String
    validate ds
      | areaCodeValid ds && exchangeValid ds = Just ds
      | otherwise                           = Nothing

    areaCodeValid :: String -> Bool
    areaCodeValid (a:_) = a `elem` ['2'..'9']
    areaCodeValid _     = False   -- Shouldn't happen with correct length.

    exchangeValid :: String -> Bool
    exchangeValid ds = case drop 3 ds of
                         (e:_) -> e `elem` ['2'..'9']
                         _     -> False   -- Shouldn't happen with correct length.
