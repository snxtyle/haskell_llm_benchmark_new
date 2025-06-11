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
  , complex
  ) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P (exp)

-- Data definition -------------------------------------------------------------
-- | A complex number with real part @a@ and imaginary part @b@.
data Complex a = Complex a a deriving (Eq, Show)

-- | Construct a complex number from a pair @(real, imaginary)@.
complex :: (a, a) -> Complex a
complex (a, b) = Complex a b

-- unary operators -------------------------------------------------------------
-- | Conjugate of a complex number:  conj (a + i·b) = a − i·b
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (-b)

-- | Absolute value (magnitude):  |a + i·b| = sqrt (a² + b²)
abs :: Floating a => Complex a -> a
abs (Complex a b) = sqrt (a * a + b * b)

-- | Real part accessor
real :: Num a => Complex a -> a
real (Complex a _) = a

-- | Imaginary part accessor
imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

-- | Complex exponential using Euler's formula:
--   e^(a + i·b) = e^a · (cos b + i·sin b)
exp :: Floating a => Complex a -> Complex a
exp (Complex a b) =
  let ea = P.exp a
  in Complex (ea * cos b) (ea * sin b)

-- binary operators ------------------------------------------------------------
-- | Multiplication of complex numbers.
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex (a * c - b * d) (b * c + a * d)

-- | Addition of complex numbers.
add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

-- | Subtraction of complex numbers.
sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

-- | Division of complex numbers.
div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
  let denom = c * c + d * d
      realPart = (a * c + b * d) / denom
      imagPart = (b * c - a * d) / denom
  in Complex realPart imagPart
