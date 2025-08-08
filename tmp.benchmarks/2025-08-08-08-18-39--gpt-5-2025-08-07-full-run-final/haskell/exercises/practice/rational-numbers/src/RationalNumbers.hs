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
data Rational a = Rational !a !a deriving (Eq, Show)

-- Internal smart constructor to ensure standard reduced form
mk :: Integral a => a -> a -> Rational a
mk _ 0 = error "Denominator cannot be zero"
mk a b =
  let sign = if b < 0 then -1 else 1
      a' = a * sign
      b' = b * sign
      g  = gcd a' b'
  in Rational (a' `quot` g) (b' `quot` g)

rational :: Integral a => (a, a) -> Rational a
rational (a, b) = mk a b

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational a b) = Rational (if a < 0 then negate a else a) b

numerator :: Integral a => Rational a -> a
numerator (Rational a _) = a

denominator :: Integral a => Rational a -> a
denominator (Rational _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = mk (a1 * b2 + a2 * b1) (b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = mk (a1 * b2 - a2 * b1) (b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = mk (a1 * a2) (b1 * b2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a1 b1) (Rational a2 b2)
  | a2 == 0   = error "Division by zero"
  | otherwise = mk (a1 * b2) (b1 * a2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational a b) n
  | n == 0          = mk 1 1
  | n > 0           = mk (a P.^ n) (b P.^ n)
  | a == 0          = error "Negative power of zero is undefined"
  | otherwise       = let m = P.abs n in mk (b P.^ m) (a P.^ m)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational a b) x = (fromIntegral a ** x) / (fromIntegral b ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational a b) = x ** (fromIntegral a / fromIntegral b)
