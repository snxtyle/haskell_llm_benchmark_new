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

import Prelude hiding (div)
import Data.Ratio (numerator, denominator, (%))
import GHC.Real (Ratio(..))

-- Data definition -------------------------------------------------------------
type Rational a = Ratio a

rational :: Integral a => (a, a) -> Rational a
rational (num, den)
  | den == 0 = error "Denominator cannot be zero"
  | otherwise = reduced (num, den)
  where
    reduced (n, d) = 
      let g = gcd n d
          n' = n `div` g
          d' = d `div` g
      in if d' < 0 then (-n') % (-d') else n' % d'

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs r = rational (abs' (numerator r), denominator r)
  where
    abs' x = if x < 0 then -x else x

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add r1 r2 = rational (n1 * d2 + n2 * d1, d1 * d2)
  where
    n1 = numerator r1
    d1 = denominator r1
    n2 = numerator r2
    d2 = denominator r2

sub :: Integral a => Rational a -> Rational a -> Rational a
sub r1 r2 = rational (n1 * d2 - n2 * d1, d1 * d2)
  where
    n1 = numerator r1
    d1 = denominator r1
    n2 = numerator r2
    d2 = denominator r2

mul :: Integral a => Rational a -> Rational a -> Rational a
mul r1 r2 = rational (n1 * n2, d1 * d2)
  where
    n1 = numerator r1
    d1 = denominator r1
    n2 = numerator r2
    d2 = denominator r2

div :: Integral a => Rational a -> Rational a -> Rational a
div r1 r2
  | n2 == 0 = error "Division by zero"
  | otherwise = rational (n1 * d2, d1 * n2)
  where
    n1 = numerator r1
    d1 = denominator r1
    n2 = numerator r2
    d2 = denominator r2

pow :: Integral a => Rational a -> a -> Rational a
pow r n
  | n >= 0 = rational (numerator r ^ n, denominator r ^ n)
  | otherwise = rational (denominator r ^ (-n), numerator r ^ (-n))

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational r x = (fromIntegral (numerator r) ** x) / (fromIntegral (denominator r) ** x)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x r = x ** (fromIntegral (numerator r) / fromIntegral (denominator r))
