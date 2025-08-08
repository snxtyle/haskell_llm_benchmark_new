module Phone (number) where

import Data.Char (isAlpha)

number :: String -> Maybe String
number xs
  | any isAlpha xs = Nothing
  | otherwise =
      case normalizeDigits xs of
        Just ds -> validate ds
        Nothing -> Nothing

-- Keep only ASCII digits and normalize optional country code '1'
normalizeDigits :: String -> Maybe String
normalizeDigits xs =
  let ds = filter isAsciiDigit xs
  in case ds of
       '1':rest | length rest == 10 -> Just rest
       _ | length ds == 10          -> Just ds
       _                            -> Nothing

validate :: String -> Maybe String
validate ds =
  case ds of
    a:b:c:d:e:f:g:h:i:j:[] ->
      if validN a && validN d
        then Just ds
        else Nothing
    _ -> Nothing

validN :: Char -> Bool
validN c = c >= '2' && c <= '9'

isAsciiDigit :: Char -> Bool
isAsciiDigit c = c >= '0' && c <= '9'
