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
data Rational a = R a a deriving (Eq)

-- We will use a smart constructor `rational` to enforce invariants:
--  - denominator is non-zero
--  - fraction is reduced to lowest terms
--  - denominator is positive (standard form)
--  - zero is represented as 0/1

-- Internal helper to construct without normalization
rationalRaw :: a -> a -> Rational a
rationalRaw = R

-- Smart constructor with normalization
rational :: Integral a => (a, a) -> Rational a
rational (a, b)
  | b == 0 = error "Denominator cannot be zero"
  | a == 0 = rationalRaw 0 1
  | otherwise =
      let g = P.gcd a b
          a' = a `P.div` g
          b' = b `P.div` g
          -- ensure denominator positive
          (n, d) = if b' < 0 then (-a', -b') else (a', b')
      in rationalRaw n d

-- Extractors
numerator :: Integral a => Rational a -> a
numerator (R n _) = n

denominator :: Integral a => Rational a -> a
denominator (R _ d) = d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs r = rational (P.abs (numerator r), P.abs (denominator r))

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add r1 r2 =
  let a1 = numerator r1
      b1 = denominator r1
      a2 = numerator r2
      b2 = denominator r2
  in rational (a1 * b2 + a2 * b1, b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub r1 r2 =
  let a1 = numerator r1
      b1 = denominator r1
      a2 = numerator r2
      b2 = denominator r2
  in rational (a1 * b2 - a2 * b1, b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul r1 r2 =
  rational (numerator r1 * numerator r2, denominator r1 * denominator r2)

div :: Integral a => Rational a -> Rational a -> Rational a
div _ r2 | numerator r2 == 0 = error "Division by zero"
div r1 r2 =
  rational (numerator r1 * denominator r2, denominator r1 * numerator r2)

pow :: Integral a => Rational a -> a -> Rational a
pow r n
  | n == 0 = rational (1,1)
  | n > 0  = rational (numPow (numerator r) n, numPow (denominator r) n)
  | otherwise =
      let m = P.abs n
          a = numerator r
          b = denominator r
      in if a == 0
         then error "Zero cannot be raised to a negative power"
         else rational (numPow b m, numPow a m)
  where
    numPow :: Integral a => a -> a -> a
    numPow base expn = P.fromInteger (P.toInteger base P.^ P.toInteger expn)

-- Real exponentiation forms ---------------------------------------------------

-- Raise a rational to a real (floating) power: (a^x)/(b^x)
expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational r x =
  let a = P.fromIntegral (numerator r)
      b = P.fromIntegral (denominator r)
  in (a P.** x) P./ (b P.** x)

-- Raise a real to a rational power: x^(a/b)
expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x r =
  let a = P.fromIntegral (numerator r)
      b = P.fromIntegral (denominator r)
  in x P.** (a P./ b)

-- Custom Show instance to display as "a/b"
instance (Show a, Integral a) => Show (Rational a) where
  show r = show (numerator r) ++ "/" ++ show (denominator r)
