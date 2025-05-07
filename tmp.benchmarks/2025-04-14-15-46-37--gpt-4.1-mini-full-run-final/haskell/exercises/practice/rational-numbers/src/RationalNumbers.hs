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
import qualified Prelude as P (abs, gcd, (^), exp)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq, Show)

-- Helper function to reduce a rational number to lowest terms and standard form
reduce :: Integral a => a -> a -> Rational a
reduce 0 0 = error "Numerator and denominator cannot both be zero"
reduce a b
  | b == 0 = Rational (signum a) 0  -- Represent infinity with denominator 0 and numerator sign
  | otherwise =
      let g = P.gcd a b
          (na, nb) = (a `div` g, b `div` g)
          -- Ensure denominator is positive
          (finalN, finalD) = if nb < 0 then (-na, -nb) else (na, nb)
      in Rational finalN finalD

rational :: Integral a => (a, a) -> Rational a
rational (a, b) = reduce a b

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational a b) = Rational (P.abs a) (P.abs b)

numerator :: Integral a => Rational a -> a
numerator (Rational a _) = a

denominator :: Integral a => Rational a -> a
denominator (Rational _ b) = b

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = reduce (a1 * b2 + a2 * b1) (b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = reduce (a1 * b2 - a2 * b1) (b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = reduce (a1 * a2) (b1 * b2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a1 b1) (Rational a2 b2)
  | a2 == 0 = error "Division by zero rational number"
  | otherwise = reduce (a1 * b2) (b1 * a2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational a b) n
  | n == 0 = Rational 1 1
  | n > 0 = reduce (a P.^ n) (b P.^ n)
  | n < 0 =
      if a == 0 then error "Zero cannot be raised to a negative power"
      else reduce (b P.^ P.abs n) (a P.^ P.abs n)

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational a b) x = (fromIntegral a) ** x / (fromIntegral b) ** x

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational a b) = x ** (fromIntegral a / fromIntegral b)
