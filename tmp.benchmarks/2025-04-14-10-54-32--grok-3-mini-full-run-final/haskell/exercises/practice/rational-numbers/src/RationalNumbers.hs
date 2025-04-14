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

import Prelude as P hiding (div, abs, Rational)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq, Show)

-- Helper function to reduce a fraction
reduce :: Integral a => a -> a -> (a, a)
reduce num den
  | den == 0    = error "Denominator cannot be zero"
  | otherwise   = let g = gcd num den  -- gcd works with negatives, but result is positive
                      num' = num `div` g
                      den' = den `div` g
                  in if den' < 0
                     then (-num', -den')  -- Make denominator positive
                     else (num', den')

-- Constructor function
rational :: Integral a => (a, a) -> Rational a
rational (num, den) = let (numReduced, denReduced) = reduce num den
                      in Rational numReduced denReduced

-- Unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational num den) = rational (P.abs num, P.abs den)  -- Use P.abs to refer to Prelude.abs

numerator :: Integral a => Rational a -> a
numerator (Rational num _) = num

denominator :: Integral a => Rational a -> a
denominator (Rational _ den) = den

-- Binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational num1 den1) (Rational num2 den2) =
  rational (num1 * den2 + num2 * den1, den1 * den2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational num1 den1) (Rational num2 den2) =
  rational (num1 * den2 - num2 * den1, den1 * den2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational num1 den1) (Rational num2 den2) =
  rational (num1 * num2, den1 * den2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational num1 den1) (Rational num2 den2)
  | num2 == 0 = error "Division by zero (numerator of second rational is zero)"
  | otherwise = rational (num1 * den2, den1 * num2)  -- Note: reduce will handle any further simplification

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational num den) exponent
  | exponent >= 0 = rational (num ^ exponent, den ^ exponent)
  | otherwise     = rational (den ^ P.abs exponent, num ^ P.abs exponent)  -- Use P.abs for Prelude.abs

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational num den) power = (fromIntegral num ** power) / (fromIntegral den ** power)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal base (Rational num den) = base ** (fromIntegral num / fromIntegral den)
