module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | invalidSides = Illegal
  | allEqual     = Equilateral
  | twoEqual     = Isosceles
  | otherwise    = Scalene
  where
    invalidSides = any (<= 0) [a, b, c] || not triangleInequality
    triangleInequality = a + b >= c && b + c >= a && a + c >= b
    allEqual = a == b && b == c
    twoEqual = a == b || b == c || a == c
