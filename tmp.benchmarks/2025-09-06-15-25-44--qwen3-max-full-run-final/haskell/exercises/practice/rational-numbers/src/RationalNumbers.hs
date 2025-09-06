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
 expReal) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

-- Create a rational number and reduce it to lowest terms
rational :: Integral a => (a, a) -> Rational a
rational (a, b)
  | b == 0 = error "Denominator cannot be zero"
  | b < 0 = rational (-a, -b)  -- Ensure denominator is positive
  | otherwise = 
    let g = gcd a b
        reducedNum = a `div` g
        reducedDen = b `div` g
    in RationalNumbers.Rational reducedNum reducedDen

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (RationalNumbers.Rational a b) = rational (Prelude.abs a, Prelude.abs b)

numerator :: Integral a => Rational a -> a
numerator (RationalNumbers.Rational a _) = a

denominator :: Integral a => Rational a -> a
denominator (RationalNumbers.Rational _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (RationalNumbers.Rational a1 b1) (RationalNumbers.Rational a2 b2) = 
  rational (a1 * b2 + a2 * b1, b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (RationalNumbers.Rational a1 b1) (RationalNumbers.Rational a2 b2) = 
  rational (a1 * b2 - a2 * b1, b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (RationalNumbers.Rational a1 b1) (RationalNumbers.Rational a2 b2) = 
  rational (a1 * a2, b1 * b2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (RationalNumbers.Rational a1 b1) (RationalNumbers.Rational a2 b2)
  | a2 == 0 = error "Cannot divide by zero"
  | otherwise = rational (a1 * b2, a2 * b1)

pow :: Integral a => Rational a -> a -> Rational a
pow _ 0 = rational (1, 1)
pow (RationalNumbers.Rational a b) n
  | n > 0 = rational (a^n, b^n)
  | otherwise = rational (b^m, a^m)
  where m = Prelude.abs n

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (RationalNumbers.Rational a b) x = (fromIntegral a ** x) / (fromIntegral b ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (RationalNumbers.Rational a b) = (x ** fromIntegral a) ** (1 / fromIntegral b)
