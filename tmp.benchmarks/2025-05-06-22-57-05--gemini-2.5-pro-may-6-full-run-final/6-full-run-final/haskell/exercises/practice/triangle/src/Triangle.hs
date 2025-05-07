module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    -- All sides must be of length > 0.
    | a <= 0 || b <= 0 || c <= 0 = Illegal
    -- The sum of the lengths of any two sides must be greater than or equal to
    -- the length of the third side.
    -- So, if a + b < c, or b + c < a, or a + c < b, it's illegal.
    | a + b < c || b + c < a || a + c < b = Illegal
    -- An equilateral triangle has all three sides the same length.
    | a == b && b == c = Equilateral
    -- An isosceles triangle has at least two sides the same length.
    -- (This check comes after the equilateral check, so if it's equilateral,
    -- it's already been classified. This branch will catch triangles with
    -- exactly two equal sides.)
    | a == b || b == c || a == c = Isosceles
    -- A scalene triangle has all sides of different lengths.
    | otherwise = Scalene
