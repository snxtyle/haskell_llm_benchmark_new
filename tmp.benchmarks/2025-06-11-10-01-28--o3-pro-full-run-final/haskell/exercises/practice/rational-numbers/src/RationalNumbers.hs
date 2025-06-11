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

import Prelude hiding (abs, Rational, div)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
-- Invariant:
--   * denominator is always strictly positive
--   * fraction is always reduced to its lowest terms
data Rational a = Rational !a !a deriving (Eq)

instance (Integral a, Show a) => Show (Rational a) where
  show r = show (numerator r) ++ "/" ++ show (denominator r)

-- Helper to build a canonical rational ---------------------------------------
canonicalize :: Integral a => a -> a -> Rational a
canonicalize _ 0 = error "Denominator cannot be zero"
canonicalize n d
  | n == 0    = Rational 0 1
  | otherwise = Rational n' d'
  where
    -- ensure positive denominator
    (n1, d1)  = if d < 0 then (-n, -d) else (n, d)
    g         = P.gcd (P.abs n1) (P.abs d1)
    n'        = n1 `P.div` g
    d'        = d1 `P.div` g

-- Public constructor ----------------------------------------------------------
rational :: Integral a => (a, a) -> Rational a
rational (n, d) = canonicalize n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = rational (P.abs n, d)

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

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
mul r1 r2 = rational (numerator r1 * numerator r2, denominator r1 * denominator r2)

div :: Integral a => Rational a -> Rational a -> Rational a
div _ (Rational 0 _) = error "Division by zero"
div r1 r2 = rational (numerator r1 * denominator r2, denominator r1 * numerator r2)

pow :: Integral a => Rational a -> a -> Rational a
pow r e
  | e == 0    = rational (1, 1)
  | e > 0     = rational (n ^ e, d ^ e)
  | otherwise = rational (d ^ eAbs, n ^ eAbs)
  where
    n     = numerator r
    d     = denominator r
    eAbs  = P.abs e

-- exponentiation helpers ------------------------------------------------------
expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational r x =
  (fromIntegral (numerator r) ** x) / (fromIntegral (denominator r) ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x r =
  x ** (fromIntegral (numerator r) / fromIntegral (denominator r))
