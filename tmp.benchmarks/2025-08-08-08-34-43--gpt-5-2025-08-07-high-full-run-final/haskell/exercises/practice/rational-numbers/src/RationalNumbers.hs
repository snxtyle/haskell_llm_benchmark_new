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
data Rational a = R a a deriving (Eq, Show)

-- Internal helper: create a normalized rational in standard form
-- - reduced by gcd
-- - denominator positive
normalize :: Integral a => a -> a -> Rational a
normalize _ 0 = error "Denominator must not be zero"
normalize n d =
  let g  = P.gcd n d
      n' = P.div n g
      d' = P.div d g
      (n'', d'') = if d' P.< 0 then (P.negate n', P.negate d') else (n', d')
  in R n'' d''

rational :: Integral a => (a, a) -> Rational a
rational (n, d) = normalize n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (R n d) = R (P.abs n) d

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
pow (R n d) e
  | e P.== 0  = R 1 1
  | e P.> 0   = rational (n P.^ e, d P.^ e)
  | otherwise = rational (d P.^ (P.abs e), n P.^ (P.abs e))

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (R n d) x =
  let base = P.fromIntegral n / P.fromIntegral d
  in base ** x

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (R n d) = x ** (P.fromIntegral n / P.fromIntegral d)
