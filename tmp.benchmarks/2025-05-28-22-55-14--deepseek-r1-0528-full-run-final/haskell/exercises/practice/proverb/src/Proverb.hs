module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite xs = 
    let middle = [ "For want of a " ++ a ++ " the " ++ b ++ " was lost." | (a,b) <- zip xs (tail xs) ]
        lastLine = "And all for the want of a " ++ head xs ++ "."
    in concatMap (++ "\n") middle ++ lastLine
