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

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (x, y) = Complex x y

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex x y) = Complex x (-y)

abs :: Floating a => Complex a -> a
abs (Complex x y) = sqrt (x * x + y * y)

real :: Num a => Complex a -> a
real (Complex x _) = x

imaginary :: Num a => Complex a -> a
imaginary (Complex _ y) = y

exp :: Floating a => Complex a -> Complex a
exp (Complex x y) = Complex (Prelude.exp x * cos y) (Prelude.exp x * sin y)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex x1 y1) (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (y1 * x2 + x1 * y2)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex x1 y1) (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex x1 y1) (Complex x2 y2) = Complex (x1 - x2) (y1 - y2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex x1 y1) (Complex x2 y2) = Complex realPart imagPart
  where
    denominator = x2 * x2 + y2 * y2
    realPart = (x1 * x2 + y1 * y2) / denominator
    imagPart = (y1 * x2 - x1 * y2) / denominator
