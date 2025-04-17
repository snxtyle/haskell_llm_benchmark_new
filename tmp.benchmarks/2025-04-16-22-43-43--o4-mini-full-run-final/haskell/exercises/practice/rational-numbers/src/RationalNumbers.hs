module RationalNumbers
  ( Rational
  , abs
  , numerator
  , denominator
  , add
  , sub
  , mul
  , div
  , pow
  , expRational
  , expReal
  , rational
  ) where

import Prelude hiding (abs, div, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
-- Rational numbers always stored in reduced form with positive denominator.
data Rational a = Rational a a deriving (Eq, Show)

-- Construct and normalize a rational number
rational :: Integral a => (a, a) -> Rational a
rational (n, d)
  | d == 0    = error "Denominator cannot be zero"
  | otherwise = let
      sign = if d < 0 then -1 else 1
      n'   = n * sign
      d'   = d * sign
      g    = P.gcd n' d'
    in Rational (n' `quot` g) (d' `quot` g)

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
div _           (Rational 0 _) = error "Cannot divide by zero"
div (Rational n1 d1) (Rational n2 d2) =
  rational (n1 * d2, d1 * n2)

-- integer power ---------------------------------------------------------------
pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) e
  | e >= 0    = rational (n ^ e, d ^ e)
  | otherwise = let m = P.abs e
                in rational (d ^ m, n ^ m)

-- rational to real power -----------------------------------------------------
expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational r x =
  let n = fromIntegral (numerator r)
      d = fromIntegral (denominator r)
  in (n / d) ** x

-- real to rational power -----------------------------------------------------
expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x r =
  let n = fromIntegral (numerator r)
      d = fromIntegral (denominator r)
  in x ** (n / d)
