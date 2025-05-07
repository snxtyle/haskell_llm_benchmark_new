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
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a
  deriving (Eq, Show)

-- Smart constructor -----------------------------------------------------------
complex :: (a, a) -> Complex a
complex = uncurry Complex

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex x y) = Complex x (- y)

abs :: Floating a => Complex a -> a
abs (Complex x y) = P.sqrt (x * x + y * y)

real :: Num a => Complex a -> a
real (Complex x _) = x

imaginary :: Num a => Complex a -> a
imaginary (Complex _ y) = y

exp :: Floating a => Complex a -> Complex a
exp (Complex x y) =
  let ex = P.exp x
  in Complex (ex * P.cos y) (ex * P.sin y)

-- binary operators ------------------------------------------------------------
add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a b) (Complex c d) = Complex (a - c) (b - d)

mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) =
  Complex (a * c - b * d) (b * c + a * d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) =
  let denom = c * c + d * d
  in Complex ((a * c + b * d) / denom)
             ((b * c - a * d) / denom)
