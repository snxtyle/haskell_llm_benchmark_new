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
import qualified Prelude as P -- To access Prelude's functions like gcd, (^), abs, div

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq, Show)

-- Helper function to reduce a rational number to its lowest terms and ensure positive denominator
reduce :: Integral a => (a, a) -> (a, a)
reduce (n, d) =
    let commonDivisor = P.gcd n d
        reducedN = n `P.div` commonDivisor
        reducedD = d `P.div` commonDivisor
    in if reducedD < 0
       then (-reducedN, -reducedD)
       else (reducedN, reducedD)

rational :: Integral a => (a, a) -> Rational a
rational (n, d)
    | d == 0    = error "Denominator cannot be zero"
    | otherwise = let (reducedN, reducedD) = reduce (n, d)
                  in Rational reducedN reducedD

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = rational (P.abs n, d) -- Denominator is already positive due to 'rational' constructor

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) =
    rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) =
    rational (n1 * d2 - n2 * d1, d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) =
    rational (n1 * n2, d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2)
    | n2 == 0   = error "Cannot divide by zero"
    | otherwise = rational (n1 * d2, n2 * d1)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) p
    | p == 0    = rational (1, 1)
    | p > 0     = rational (n P.^ p, d P.^ p)
    | p < 0     =
        let absP = P.abs p
        in if n == 0
           then error "Cannot raise zero to a negative power"
           else rational (d P.^ absP, n P.^ absP)

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (Rational n d) x =
    (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x (Rational n d) =
    (x ** fromIntegral n) ** (1 / fromIntegral d)
