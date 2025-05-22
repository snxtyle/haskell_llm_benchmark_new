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

import Prelude hiding (div, Rational, gcd)
import qualified Prelude

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

-- Helper function for GCD
gcd :: Integral a => a -> a -> a
gcd a 0 = Prelude.abs a
gcd a b = gcd b (a `mod` b)

-- Helper function to reduce and normalize rational numbers
reduce :: Integral a => a -> a -> Rational a
reduce n d
  | d == 0 = error "Denominator cannot be zero"
  | d < 0 = reduce (-n) (-d)  -- Ensure positive denominator
  | otherwise = Rational (n `quot` g) (d `quot` g)
  where g = gcd n d

rational :: Integral a => (a, a) -> Rational a
rational (n, d) = reduce n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (Prelude.abs n) d

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = reduce (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = reduce (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = reduce (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2) 
  | n2 == 0 = error "Division by zero"
  | otherwise = reduce (n1 * d2) (d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) p
  | p == 0 = Rational 1 1
  | p > 0 = reduce (n ^ p) (d ^ p)
  | otherwise = reduce (d ^ (-p)) (n ^ (-p))

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational n d) = x ** (fromIntegral n / fromIntegral d)
