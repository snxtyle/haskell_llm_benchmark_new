module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not (a > 0 && b > 0 && c > 0) = Illegal -- All sides must be greater than 0
  | not (a + b >= c && b + c >= a && a + c >= b) = Illegal -- Triangle inequality
  | a == b && b == c = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise = Scalene
