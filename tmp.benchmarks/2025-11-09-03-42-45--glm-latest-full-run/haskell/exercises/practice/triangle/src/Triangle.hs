module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  -- A triangle is illegal if any side is non-positive.
  | a <= 0 || b <= 0 || c <= 0 = Illegal
  -- A triangle is illegal if it violates the triangle inequality theorem.
  | a + b < c || b + c < a || a + c < b = Illegal
  -- An equilateral triangle has all three sides the same length.
  | a == b && b == c = Equilateral
  -- An isosceles triangle has at least two sides the same length.
  | a == b || b == c || a == c = Isosceles
  -- A scalene triangle has all sides of different lengths.
  | otherwise = Scalene
