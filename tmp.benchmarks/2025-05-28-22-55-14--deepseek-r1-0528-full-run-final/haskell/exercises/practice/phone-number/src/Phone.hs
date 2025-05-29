module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let cleaned = filter isDigit xs
        candidate = case cleaned of
            '1' : rest@[_,_,_,_,_,_,_,_,_,_] -> rest
            p@[_,_,_,_,_,_,_,_,_,_]           -> p
            _                                  -> []
    in case candidate of
        [a,_,_,b,_,_,_,_,_,_] 
            | a `elem` "23456789" && b `elem` "23456789" -> Just candidate
        _ -> Nothing
