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
import qualified Prelude (exp) -- For using Prelude.exp

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (-i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt (r*r + i*i)

real :: Num a => Complex a -> a
real (Complex r _) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

exp :: Floating a => Complex a -> Complex a
-- e^(a + ib) = e^a * e^(ib) = e^a * (cos b + i * sin b)
exp (Complex a b) = Complex (Prelude.exp a * cos b) (Prelude.exp a * sin b)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
-- (a + ib) * (c + id) = (ac - bd) + i(bc + ad)
mul (Complex a b) (Complex c d) = Complex (a*c - b*d) (b*c + a*d)

add :: Num a => Complex a -> Complex a -> Complex a
-- (a + ib) + (c + id) = (a + c) + i(b + d)
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
-- (a + ib) - (c + id) = (a - c) + i(b - d)
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
-- (a + ib) / (c + id) = (ac + bd)/(c^2 + d^2) + i(bc - ad)/(c^2 + d^2)
div (Complex a b) (Complex c d) =
  let denominator = c*c + d*d
  in Complex ((a*c + b*d) / denominator) ((b*c - a*d) / denominator)
