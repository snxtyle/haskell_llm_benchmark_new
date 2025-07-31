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

import Prelude (Eq, Show, ($), (.), (==))
import qualified Prelude as P hiding (abs, div)

-- Data definition -------------------------------------------------------------
-- Represent a complex number as real and imaginary parts.
data Complex a = Complex { real :: a, imaginary :: a }
  deriving (Eq, Show)

-- Construct a complex number from a tuple (real, imaginary).
complex :: (a, a) -> Complex a
complex (a, b) = Complex a b

-- unary operators -------------------------------------------------------------
-- Conjugate: a + b i -> a - b i
conjugate :: P.Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (P.negate b)

-- Absolute value (magnitude): |z| = sqrt(a^2 + b^2)
abs :: P.Floating a => Complex a -> a
abs (Complex a b) = P.sqrt (a P.* a P.+ b P.* b)

-- Exponent: e^(a + i b) = e^a * (cos b + i sin b)
exp :: P.Floating a => Complex a -> Complex a
exp (Complex a b) =
  let ea = P.exp a
  in Complex (ea P.* P.cos b) (ea P.* P.sin b)

-- binary operators ------------------------------------------------------------
-- Multiplication: (a + i b) * (c + i d) = (ac - bd) + (bc + ad) i
mul :: P.Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) =
  Complex (a P.* c P.- b P.* d) (b P.* c P.+ a P.* d)

-- Addition: (a + i b) + (c + i d) = (a + c) + (b + d) i
add :: P.Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a P.+ c) (b P.+ d)

-- Subtraction: (a + i b) - (c + i d) = (a - c) + (b - d) i
sub :: P.Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a P.- c) (b P.- d)

-- Division:
-- (a + i b) / (c + i d) = ((a c + b d) / (c^2 + d^2)) + ((b c - a d) / (c^2 + d^2)) i
div :: P.Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
  let denom = c P.* c P.+ d P.* d
      realPart = (a P.* c P.+ b P.* d) P./ denom
      imagPart = (b P.* c P.- a P.* d) P./ denom
  in Complex realPart imagPart
