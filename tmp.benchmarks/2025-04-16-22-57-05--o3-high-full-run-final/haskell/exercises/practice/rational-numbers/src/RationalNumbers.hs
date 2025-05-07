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

import Prelude hiding (div, abs, Rational)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
-- The invariant for every `Rat n d` value is:
--   * d > 0        (denominator is always positive)
--   * gcd n d == 1 (fraction is reduced)
data Rational a = Rat a a deriving (Eq)

instance Show a => Show (Rational a) where
  show (Rat n d) = show n ++ "/" ++ show d

-- Helpers ---------------------------------------------------------------------
normalize :: Integral a => a -> a -> Rational a
normalize _ 0 = error "Denominator cannot be zero"
normalize n d =
  let g             = P.gcd n d
      n'            = n `P.div` g
      d'            = d `P.div` g
      (nStd, dStd)  = if d' < 0 then (-n', -d') else (n', d')
  in Rat nStd dStd

-- Constructor -----------------------------------------------------------------
rational :: Integral a => (a, a) -> Rational a
rational (n, d) = normalize n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rat n d) = Rat (P.abs n) d   -- `d` is already positive by invariant

numerator :: Integral a => Rational a -> a
numerator (Rat n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rat _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rat n1 d1) (Rat n2 d2) = normalize (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rat n1 d1) (Rat n2 d2) = normalize (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rat n1 d1) (Rat n2 d2) = normalize (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div _            (Rat 0 _) = error "Division by zero"
div (Rat n1 d1) (Rat n2 d2) = normalize (n1 * d2) (d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rat n d) e
  | e == 0    = Rat 1 1
  | e > 0     = normalize (n P.^ e) (d P.^ e)
  | otherwise = let e' = P.abs e
                in normalize (d P.^ e') (n P.^ e')

-- | Raise a Rational to a real (floating‑point) power.
expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rat n d) x =
  (P.fromIntegral n P.** x) / (P.fromIntegral d P.** x)

-- | Raise a real (floating‑point) number to a Rational power.
expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rat n d) = x P.** (P.fromIntegral n / P.fromIntegral d)
