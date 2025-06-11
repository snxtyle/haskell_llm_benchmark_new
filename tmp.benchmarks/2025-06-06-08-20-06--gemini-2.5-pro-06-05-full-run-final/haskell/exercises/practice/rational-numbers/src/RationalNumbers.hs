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
data Rational a = R a a deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (n, d)
    | d == 0 = error "Denominator must not be zero"
    | d < 0  = rational (negate n, negate d)
    | otherwise =
        let common = gcd n d
        in R (n `div` common) (d `div` common)

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (R n d) = rational (P.abs n, d)

numerator :: Integral a => Rational a -> a
numerator (R n _) = n

denominator :: Integral a => Rational a -> a
denominator (R _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (R n1 d1) (R n2 d2) = rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (R n1 d1) (R n2 d2) = rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (R n1 d1) (R n2 d2) = rational (n1 * n2, d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (R n1 d1) (R n2 d2) = rational (n1 * d2, n2 * d1)

pow :: Integral a => Rational a -> a -> Rational a
pow (R n d) p
    | p >= 0    = rational (n ^ p, d ^ p)
    | otherwise = rational (d ^ P.abs p, n ^ P.abs p)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (R n d) x = (fromIntegral n / fromIntegral d) ** x

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (R n d) = x ** (fromIntegral n / fromIntegral d)
