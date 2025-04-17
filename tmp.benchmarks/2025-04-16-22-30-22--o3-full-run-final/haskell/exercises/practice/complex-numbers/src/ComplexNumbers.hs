module ComplexNumbers
( Complex
, conjugate
, abs
, exp
, real
, imaginary
, mul
, add
, sub
, div
, complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P
  ( abs
  , sqrt
  , exp
  , cos
  , sin )

-- Data definition -------------------------------------------------------------
-- | A complex number in the form a + b * i
data Complex a = Complex a a deriving (Eq, Show)

-- | Smart‑constructor from a tuple.
complex :: (a, a) -> Complex a
complex (a, b) = Complex a b

-- unary operators -------------------------------------------------------------
-- | Conjugate of a complex number: a - b * i
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (negate b)

-- | Absolute value (magnitude) of a complex number.
abs :: Floating a => Complex a -> a
abs (Complex a b) = P.sqrt (a * a + b * b)

-- | Real part.
real :: Num a => Complex a -> a
real (Complex a _) = a

-- | Imaginary part.
imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

-- | Exponential of a complex number using Euler's formula.
exp :: Floating a => Complex a -> Complex a
exp (Complex a b) = Complex (eA * P.cos b) (eA * P.sin b)
  where
    eA = P.exp a

-- binary operators ------------------------------------------------------------
-- | Multiplication of two complex numbers.
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) =
  Complex (a * c - b * d) (b * c + a * d)

-- | Addition of two complex numbers.
add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

-- | Subtraction of two complex numbers.
sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

-- | Division of two complex numbers.
div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
  Complex realPart imagPart
  where
    denom     = c * c + d * d
    realPart  = (a * c + b * d) / denom
    imagPart  = (b * c - a * d) / denom
