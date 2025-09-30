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

import Prelude hiding (div, abs, Rational)
import qualified Prelude

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (n, d)
  | d == 0    = error "Denominator cannot be zero"
  | otherwise = Rational (signum d * n `quot` g) (Prelude.abs d `quot` g)
  where
    g = gcd n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (Prelude.abs n) d

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 + a2 * b1, b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 - a2 * b1, b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = rational (a1 * a2, b1 * b2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2, a2 * b1)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational a b) n
  | n >= 0    = rational (a ^ n, b ^ n)
  | otherwise = rational (b ^ m, a ^ m)
  where
    m = Prelude.abs n

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (Rational a b) x = (fromIntegral a ** x) / (fromIntegral b ** x)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x (Rational a b) = (x ** fromIntegral a) ** (1.0 / fromIntegral b)
