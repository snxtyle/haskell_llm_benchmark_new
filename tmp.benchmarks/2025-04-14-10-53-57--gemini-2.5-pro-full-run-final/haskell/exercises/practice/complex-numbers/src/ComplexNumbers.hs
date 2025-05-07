module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude (exp) -- Use qualified import for Prelude's exp

-- Data definition -------------------------------------------------------------
-- Represents a complex number with a real and imaginary part.
data Complex a = Complex a a deriving (Eq, Show)

-- Constructor function for Complex numbers from a tuple.
complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

-- unary operators -------------------------------------------------------------

-- Returns the complex conjugate of a complex number.
-- conjugate(a + bi) = a - bi
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (-i)

-- Returns the absolute value (or modulus) of a complex number.
-- abs(a + bi) = sqrt(a^2 + b^2)
abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt (r*r + i*i)

-- Returns the real part of a complex number.
real :: Num a => Complex a -> a
real (Complex r _) = r

-- Returns the imaginary part of a complex number.
imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

-- Returns the exponential of a complex number.
-- exp(a + bi) = exp(a) * (cos(b) + i * sin(b))
exp :: Floating a => Complex a -> Complex a
exp (Complex r i) = Complex (Prelude.exp r * cos i) (Prelude.exp r * sin i)

-- binary operators ------------------------------------------------------------

-- Multiplies two complex numbers.
-- (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)

-- Adds two complex numbers.
-- (a + bi) + (c + di) = (a + c) + (b + d)i
add :: Num a => Complex a -> Complex a -> Complex a
add (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

-- Subtracts the second complex number from the first.
-- (a + bi) - (c + di) = (a - c) + (b - d)i
sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

-- Divides the first complex number by the second.
-- (a + bi) / (c + di) = (ac + bd)/(c^2 + d^2) + (bc - ad)/(c^2 + d^2)i
div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex r1 i1) (Complex r2 i2) =
    let denom = r2*r2 + i2*i2
    in Complex ((r1*r2 + i1*i2) / denom) ((i1*r2 - r1*i2) / denom)
