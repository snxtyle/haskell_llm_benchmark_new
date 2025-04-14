module Darts (score) where

score :: Float -> Float -> Int
score x y = 
    let distance = sqrt (x * x + y * y)
    in if distance > 10 then 0    -- Outside target
       else if distance > 5 then 1 -- Outer circle
       else if distance > 1 then 5 -- Middle circle
       else 10                     -- Inner circle (bullseye)
