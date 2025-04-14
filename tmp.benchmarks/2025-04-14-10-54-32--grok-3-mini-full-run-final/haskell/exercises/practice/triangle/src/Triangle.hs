module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | a <= 0 || b <= 0 || c <= 0 = Illegal  -- All sides must be greater than 0
  | a + b < c || a + c < b || b + c < a = Illegal  -- Triangle inequality
  | a == b && b == c = Equilateral  -- All sides equal
  | a == b || b == c || a == c = Isosceles  -- At least two sides equal
  | otherwise = Scalene  -- All sides different
