module Trinary (readTri, showTri) where

-- A strict left fold, as in Data.List.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) =
  let z' = f z x
  in z' `seq` foldl' f z' xs

readTri :: String -> Integer
readTri s =
  if all isTriDigit s
  then foldl' toDecimal 0 s
  else 0
  where
    isTriDigit :: Char -> Bool
    isTriDigit c = c >= '0' && c <= '2'

    toDigit :: Char -> Integer
    toDigit c = fromIntegral (fromEnum c - fromEnum '0')

    toDecimal :: Integer -> Char -> Integer
    toDecimal acc c = acc * 3 + toDigit c

showTri :: a -> String
showTri = error "You need to implement this function."
