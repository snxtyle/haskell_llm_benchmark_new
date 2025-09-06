module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x:xs) = if not (p x) then x : discard p xs else discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x:xs) = if p x then x : keep p xs else keep p xs
