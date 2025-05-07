module ComplexNumbers
  ( Complex,
    conjugate,
    abs,
    exp,
    real,
    imaginary,
    mul,
    add,
    sub,
    div,
    complex
  ) where

import Prelude hiding (abs, div, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
-- | Internal representation keeps the real and imaginary components.
--   Constructor is *not* exported, keeping the type abstract.
data Complex a = Complex a a
  deriving (Eq, Show)

-- | Create a complex number from a pair of (real, imaginary) parts.
complex :: (a, a) -> Complex a
complex (x, y) = Complex x y

-- unary operators -------------------------------------------------------------
-- | Conjugate of a complex number:  a + i·b  ↦  a - i·b
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (-b)

-- | Absolute value (magnitude) of a complex number.
abs :: Floating a => Complex a -> a
abs (Complex a b) = P.sqrt (a * a + b * b)

-- | Real part accessor.
real :: Num a => Complex a -> a
real (Complex a _) = a

-- | Imaginary part accessor.
imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

-- | Exponential of a complex number using Euler's formula.
exp :: Floating a => Complex a -> Complex a
exp (Complex a b) =
  let eToA = P.exp a
   in Complex (eToA * P.cos b) (eToA * P.sin b)

-- binary operators ------------------------------------------------------------
-- | Multiplication of two complex numbers.
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = Complex (a * c - b * d) (b * c + a * d)

-- | Addition of two complex numbers.
add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

-- | Subtraction of two complex numbers.
sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

-- | Division of two complex numbers.
div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
  let denom = c * c + d * d
      realPart = (a * c + b * d) / denom
      imagPart = (b * c - a * d) / denom
   in Complex realPart imagPart
