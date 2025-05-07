module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

-- | Classify a triangle based on the lengths of its three sides.
--
-- An 'Illegal' triangle is one where at least one side is non‑positive
-- or where the triangle inequality does not hold.
--
-- Precedence of classifications (once the triangle is known to be legal):
--   * Equilateral  – all three sides equal
--   * Isosceles    – at least two sides equal
--   * Scalene      – all sides different
triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not positiveSides         = Illegal
  | not satisfiesInequality   = Illegal
  | a == b && b == c          = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise                 = Scalene
  where
    positiveSides = a > 0 && b > 0 && c > 0
    satisfiesInequality =
         a + b >= c
      && b + c >= a
      && a + c >= b
