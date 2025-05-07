module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

-- Helper function to check the triangle inequality and positive side lengths
isValidTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isValidTriangle a b c =
  let sides = [a, b, c]
      positiveSides = all (> 0) sides
      inequalityCheck = a + b >= c && b + c >= a && a + c >= b
  in positiveSides && inequalityCheck

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not (isValidTriangle a b c) = Illegal
  | a == b && b == c            = Equilateral
  | a == b || b == c || a == c  = Isosceles
  | otherwise                   = Scalene
