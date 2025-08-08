module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal = go 0
  where
    go :: Int -> String -> Int
    go acc [] = acc
    go acc ('0':xs) = go (acc * 2) xs
    go acc ('1':xs) = go (acc * 2 + 1) xs
    go _   _        = 0
