module Darts (score) where

score :: Float -> Float -> Int
score x y = 
  let distanceSquared = x^2 + y^2
  in if distanceSquared <= 1
       then 10
       else if distanceSquared <= 25
         then 5
         else if distanceSquared <= 100
           then 1
           else 0
