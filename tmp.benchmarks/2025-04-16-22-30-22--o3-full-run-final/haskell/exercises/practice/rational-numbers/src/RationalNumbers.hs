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
import qualified Prelude as P (abs, gcd, (**), fromIntegral)

-- Data definition -------------------------------------------------------------
-- We keep the constructor private so users must go through `rational`
data Rational a = R !a !a deriving (Eq, Show)

-- Internal helper -------------------------------------------------------------
-- Create a rational value reduced to its lowest terms and in standard form
mkRational :: Integral a => a -> a -> Rational a
mkRational _ 0 = error "Denominator cannot be zero"
mkRational n d =
  let g             = P.gcd n d
      n'            = n `quot` g
      d'            = d `quot` g
      (nStd, dStd)  = if d' < 0 then (-n', -d') else (n', d')
  in R nStd dStd

-- Public constructor ----------------------------------------------------------
rational :: Integral a => (a, a) -> Rational a
rational (n, d) = mkRational n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (R n d) = mkRational (P.abs n) (P.abs d)

numerator :: Integral a => Rational a -> a
numerator (R n _) = n

denominator :: Integral a => Rational a -> a
denominator (R _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (R n1 d1) (R n2 d2) = mkRational (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (R n1 d1) (R n2 d2) = mkRational (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (R n1 d1) (R n2 d2) = mkRational (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div _        (R 0 _ )   = error "Division by zero"
div (R n1 d1) (R n2 d2) = mkRational (n1 * d2) (n2 * d1)

-- exponentiation --------------------------------------------------------------
pow :: Integral a => Rational a -> a -> Rational a
pow (R _ _) 0 = mkRational 1 1
pow (R n d) e
  | e > 0     = mkRational (n ^ e) (d ^ e)
  | otherwise = mkRational (d ^ m) (n ^ m)
  where
    m = P.abs e

-- Rational raised to a real power --------------------------------------------
expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (R n d) x =
  (P.fromIntegral n P.** x) / (P.fromIntegral d P.** x)

-- Real raised to a rational power --------------------------------------------
expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (R n d) = x P.** (P.fromIntegral n / P.fromIntegral d)
