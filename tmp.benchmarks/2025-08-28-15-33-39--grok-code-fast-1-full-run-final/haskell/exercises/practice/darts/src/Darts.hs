module Darts (score) where

score :: Float -> Float -> Int
score x y = let dist = sqrt (x * x + y * y)
            in if dist <= 1 then 10
               else if dist <= 5 then 5
               else if dist <= 10 then 1
               else 0
