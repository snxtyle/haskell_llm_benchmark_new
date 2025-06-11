module Triangle (TriangleType(..), triangleType) where

-- | The four possible classifications of a triangle.
data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

-- | Classify a triangle based on the lengths of its three sides.
--
-- The classification rules are:
--
-- 1. A triangle is 'Illegal' if any side is non-positive
--    or if the triangle inequality is violated.
-- 2. An 'Equilateral' triangle has all three sides equal.
-- 3. An 'Isosceles' triangle has at least two sides equal.
-- 4. A 'Scalene' triangle has all sides of different lengths.
--
-- Note that an equilateral triangle also satisfies the isosceles
-- condition, so we must test for equilateral first.
triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<= 0) sides                 = Illegal
  | not triangleInequalitySatisfied  = Illegal
  | a == b && b == c                 = Equilateral
  | a == b || b == c || a == c       = Isosceles
  | otherwise                        = Scalene
  where
    sides = [a, b, c]
    triangleInequalitySatisfied =
      a + b >= c &&
      b + c >= a &&
      a + c >= b
