module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

-- | Determine the type of a triangle from the lengths of its three sides.
--   A triangle is
--     • Illegal    – if any side is non‑positive, or the triangle inequality
--                    is violated.
--     • Equilateral – if all three sides are equal.
--     • Isosceles   – if at least two sides are equal.
--     • Scalene     – if all three sides are different.
--
--   The function works for any numeric type that supports addition and
--   ordering (for example Int, Integer, Float, Double, etc.).
triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not positiveSides           = Illegal
  | not satisfiesInequality     = Illegal
  | allEqual                    = Equilateral
  | twoEqual                    = Isosceles
  | otherwise                   = Scalene
  where
    -- All sides must be strictly greater than zero.
    positiveSides = a > 0 && b > 0 && c > 0

    -- Triangle inequality: sum of any two sides
    -- must be ≥ the third side.
    satisfiesInequality =
         a + b >= c
      && b + c >= a
      && a + c >= b

    -- Equality test using Ord only (avoids needing Eq constraint explicitly).
    same x y = compare x y == EQ

    allEqual = same a b && same b c
    twoEqual = same a b || same b c || same a c
