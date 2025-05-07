module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational, gcd) -- Hiding Prelude.Rational, Prelude.gcd, etc.
import qualified Prelude (gcd, abs, div) -- Importing necessary Prelude functions qualified or for specific use

-- Internal data structure. The data constructor R is not exported by default
-- because Rational(..) is not used in the module export list.
data ActualRational a = R a a deriving (Show)

-- Public type alias, matching the export list 'Rational'
type Rational a = ActualRational a

-- Smart constructor: ensures canonical form.
-- 1. Denominator is positive.
-- 2. Fraction is in lowest terms.
-- 3. If numerator is 0, denominator is 1.
rational :: Integral a => (a, a) -> Rational a
rational (n, d)
    | d == 0    = error "Denominator cannot be zero"
    | n == 0    = R 0 1
    | otherwise = let common = Prelude.gcd n d
                      num    = n `Prelude.div` common -- Using Prelude.div for integer division
                      den    = d `Prelude.div` common
                  in if den < 0
                     then R (-num) (-den)
                     else R num den

-- Eq instance relies on the canonical form produced by 'rational'.
-- If two numbers are equal, their canonical forms (R n d) must be identical.
instance (Integral a, Eq a) => Eq (ActualRational a) where
  (R n1 d1) == (R n2 d2) = n1 == n2 && d1 == d2

-- Unary operators -------------------------------------------------------------

-- Absolute value of a rational number.
-- Since 'd' is guaranteed to be positive by 'rational', abs only applies to 'n'.
-- The result is already in canonical form if the input was.
abs :: Integral a => Rational a -> Rational a
abs (R n d) = R (Prelude.abs n) d

numerator :: Integral a => Rational a -> a
numerator (R n _) = n

denominator :: Integral a => Rational a -> a
denominator (R _ d) = d

-- Binary operators ------------------------------------------------------------
-- All binary operations producing a Rational number must use the 'rational'
-- smart constructor to ensure the result is in canonical form.

add :: Integral a => Rational a -> Rational a -> Rational a
add (R n1 d1) (R n2 d2) = rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (R n1 d1) (R n2 d2) = rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (R n1 d1) (R n2 d2) = rational (n1 * n2, d1 * d2)

-- Division of two rational numbers.
-- (n1/d1) / (n2/d2) = (n1*d2) / (d1*n2)
-- Error if numerator of divisor (n2) is zero.
div :: Integral a => Rational a -> Rational a -> Rational a
div (R n1 d1) r2@(R n2 d2)
    | n2 == 0   = error "Division by zero: divisor's numerator is zero"
    | otherwise = rational (n1 * d2, d1 * n2)

-- Exponentiation --------------------------------------------------------------

-- Exponentiation of a rational number to an integer power.
-- The type of the exponent 'p' is 'a', which is Integral.
pow :: Integral a => Rational a -> a -> Rational a
pow _ 0 = rational (1, 1) -- r^0 = 1/1
pow (R n d) p
    | p > 0     = rational (n ^ p, d ^ p)
    | otherwise = -- p < 0
        if n == 0
        then error "Division by zero: 0 raised to a negative power"
        else
            -- m = |p|. If p is minBound of a fixed-size type, abs p can be p.
            -- Assuming 'a' is Integer or p is not minBound for Int.
            -- Or, more robustly for negative p, use `negate p` for the exponent value,
            -- assuming `^` requires a non-negative exponent.
            -- The problem states m = |n|, so we use Prelude.abs.
            let m = Prelude.abs p
            in rational (d ^ m, n ^ m)

-- Exponentiation of a rational number to a real (floating-point) power.
-- (n/d)^x = (n^x) / (d^x)
expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (R n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

-- Exponentiation of a real number to a rational power.
-- x^(n/d)
expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (R n d) = x ** (fromIntegral n / fromIntegral d)
