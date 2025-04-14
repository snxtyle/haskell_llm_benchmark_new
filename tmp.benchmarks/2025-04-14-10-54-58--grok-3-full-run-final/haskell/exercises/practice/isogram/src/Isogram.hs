module Isogram (isIsogram) where

isIsogram :: String -> Bool
isIsogram str = let letters = filter (`elem` ['a'..'z']) (map toLower str)
                in length letters == length (nub letters)
  where
    toLower c = if c `elem` ['A'..'Z'] then toEnum (fromEnum c + 32) else c
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)
