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
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (n, d)
  | d == 0 = error "Denominator cannot be zero"
  | d < 0 = Rational (negate (n `div` gcdVal)) (negate (d `div` gcdVal))
  | otherwise = Rational (n `div` gcdVal) (d `div` gcdVal)
  where
    gcdVal = gcd (P.abs n) (P.abs d)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (P.abs n) d

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = 
  rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = 
  rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = 
  rational (n1 * n2, d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2)
  | n2 == 0 = error "Division by zero"
  | otherwise = rational (n1 * d2, d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow _ 0 = Rational 1 1
pow (Rational n d) e
  | e > 0 = Rational (n ^ e) (d ^ e)
  | otherwise = let absE = P.abs e in Rational (d ^ absE) (n ^ absE)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational n d) = x ** (fromIntegral n / fromIntegral d)
