module Darts (score) where

score :: Float -> Float -> Int
score x y =
  let d = sqrt (x * x + y * y)
  in if d <= 1
     then 10
     else if d <= 5
     then 5
     else if d <= 10
     then 1
     else 0
