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

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(Eq)

instance (Integral a, Show a) => Show (Rational a) where
    show (Rational n d) = show n ++ "/" ++ show d

rational :: Integral a => (a, a) -> Rational a
rational (n, d)
    | d == 0    = error "Denominator cannot be zero"
    | otherwise = reduce (Rational n d)

-- Helper function to calculate GCD using Euclidean algorithm
gcd' :: Integral a => a -> a -> a
gcd' a b = let a' = abs' a
               b' = abs' b
           in if b' == 0 then a' else gcd' b' (a' `mod` b')
    where abs' x = if x < 0 then -x else x

-- Helper function to reduce a rational number to lowest terms
reduce :: Integral a => Rational a -> Rational a
reduce (Rational n d) =
    let divisor = gcd' n d
        n' = n `quot` divisor
        d' = d `quot` divisor
    in if d' < 0
       then Rational (-n') (-d')
       else Rational n' d'

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = Rational (abs' n) (abs' d)
    where abs' x = if x < 0 then -x else x

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) =
    reduce $ Rational (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) =
    reduce $ Rational (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) =
    reduce $ Rational (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2)
    | n2 == 0   = error "Division by zero"
    | otherwise = reduce $ Rational (n1 * d2) (d1 * n2)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) exp
    | exp >= 0  = reduce $ Rational (n ^ exp) (d ^ exp)
    | otherwise = reduce $ Rational (d ^ absExp) (n ^ absExp)
    where absExp = if exp < 0 then -exp else exp

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (Rational n d) exp = (fromIntegral n ** exp) / (fromIntegral d ** exp)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal base (Rational n d) = (base ** fromIntegral n) ** (1 / fromIntegral d)
