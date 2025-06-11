module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    | s1 <= 0 || s1 + s2 < s3 = Illegal
    | a == b && b == c        = Equilateral
    | a == b || b == c || a == c = Isosceles
    | otherwise               = Scalene
    where
        [s1, s2, s3] = sort [a, b, c]
