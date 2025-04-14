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
import qualified Prelude -- Use qualified import for Prelude functions like abs

-- Data definition -------------------------------------------------------------
-- Using strict fields for potentially better performance
data Rational a = !a :% !a deriving (Eq)

-- Show instance for easier debugging and display
instance (Integral a, Show a) => Show (Rational a) where
    show (n :% d) = show n ++ " % " ++ show d

-- Helper function to create reduced, standard-form rational numbers
-- Ensures denominator is positive and the fraction is in lowest terms.
mkRational :: Integral a => a -> a -> Rational a
mkRational n d
  | d == 0 = error "Ratio has zero denominator"
  -- Use Prelude.gcd to find the greatest common divisor
  | g == 0 = 0 :% 1 -- This case handles n = 0, d /= 0
  | otherwise = let num = n `Prelude.div` g -- Use Prelude.div for integer division
                    den = d `Prelude.div` g -- Use Prelude.div for integer division
                in if den < 0 then (-num) :% (-den) else num :% den
  where g = Prelude.gcd n d -- Explicitly use Prelude.gcd

-- Constructor function exposed by the module
rational :: Integral a => (a, a) -> Rational a
rational (n, d) = mkRational n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
-- Denominator is guaranteed positive by mkRational, so only need abs of numerator
abs (n :% d) = mkRational (Prelude.abs n) d -- Use Prelude.abs

numerator :: Integral a => Rational a -> a
numerator (n :% _) = n

denominator :: Integral a => Rational a -> a
denominator (_ :% d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (n1 :% d1) (n2 :% d2) = mkRational (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (n1 :% d1) (n2 :% d2) = mkRational (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (n1 :% d1) (n2 :% d2) = mkRational (n1 * n2) (d1 * d2)

-- This is the div function for Rational numbers
div :: Integral a => Rational a -> Rational a -> Rational a
-- Division by r2 = n2/d2 is multiplication by d2/n2.
-- mkRational handles the case where the new denominator (d1 * n2) is zero.
div (n1 :% d1) (n2 :% d2) = mkRational (n1 * d2) (d1 * n2)

-- Exponentiation --------------------------------------------------------------

-- Exponentiation of a rational number to an integer power
pow :: Integral a => Rational a -> a -> Rational a
pow r p = case compare p 0 of
    EQ -> 1 :% 1 -- r^0 = 1
    GT -> let (n :% d) = r in mkRational (n ^ p) (d ^ p) -- r^p = (n^p) / (d^p) for p > 0
    LT -> -- r^p = (d^m) / (n^m) for p < 0, where m = |p|
        let (n :% d) = r
            m = Prelude.abs p -- Use Prelude.abs
        in if n == 0
           then error "Cannot raise zero to a negative power"
           else mkRational (d ^ m) (n ^ m)

-- Exponentiation of a rational number to a real power
expRational :: (Integral a, Floating b) => Rational a -> b -> b
-- Use fromIntegral to convert numerator and denominator before division
expRational (n :% d) x = (fromIntegral n / fromIntegral d) ** x

-- Exponentiation of a real number to a rational power
expReal :: (Floating a, Integral b) => a -> Rational b -> a
-- Use fromIntegral to convert numerator and denominator before division
expReal x (n :% d) = x ** (fromIntegral n / fromIntegral d)
