module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  let ds = filter isDigit xs
  in case ds of
       ('1':rest) | length rest == 10 -> validate rest
       _ | length ds == 10            -> validate ds
       _                              -> Nothing
  where
    validate s
      | validArea (head s) && validExchange (s !! 3) = Just s
      | otherwise                                     = Nothing

    validArea c     = c >= '2' && c <= '9'
    validExchange c = c >= '2' && c <= '9'
