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

import Prelude hiding (Rational, abs, div)

-- Data definition -------------------------------------------------------------
data Rational a = Rational !a !a deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (n, d) = reduceRational n d

-- Helper function to reduce a fraction to lowest terms with positive denominator
reduceRational :: Integral a => a -> a -> Rational a
reduceRational n d
  | d == 0 = error "Denominator cannot be zero"
  | otherwise =
      let g = gcd n d
          n' = n `quot` g
          d' = d `quot` g
      in if d' < 0 
         then Rational (negate n') (negate d')
         else Rational n' d'

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (Prelude.abs n) d

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- Helper function for reciprocal
reciprocal :: Integral a => Rational a -> Rational a
reciprocal (Rational 0 _) = error "Division by zero"
reciprocal (Rational n d) = reduceRational d n

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = 
  reduceRational (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = 
  reduceRational (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) =
  reduceRational (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div r1 r2 = mul r1 (reciprocal r2)

pow :: Integral a => Rational a -> a -> Rational a
pow r n
  | n >= 0    = Rational (numerator r ^ n) (denominator r ^ n)
  | otherwise = reciprocal (pow r (negate n))

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational r x = (fromIntegral (numerator r) ** x) / (fromIntegral (denominator r) ** x)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x r = x ** (fromIntegral (numerator r) / fromIntegral (denominator r))
