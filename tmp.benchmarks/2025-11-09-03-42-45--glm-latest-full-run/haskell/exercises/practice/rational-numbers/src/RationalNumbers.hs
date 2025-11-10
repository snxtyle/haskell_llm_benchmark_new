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

import Prelude hiding (div, Rational) -- Hide Prelude's div and Rational. abs is no longer hidden.

-- Data definition -------------------------------------------------------------
-- The constructor is not exported to ensure all Rational numbers are created
-- through the `rational` function, which handles normalization.
data Rational a = Rational a a deriving (Eq)

-- Custom Show instance to display as "numerator/denominator"
instance (Show a, Integral a) => Show (Rational a) where
    show (Rational n d) = show n ++ "/" ++ show d

-- Helper function to normalize a rational number
-- Ensures denominator is positive and the fraction is in lowest terms.
normalize :: Integral a => a -> a -> Rational a
normalize n d
    | d == 0    = Rational (signum n) 0 -- Handle infinity-like cases
    | n == 0    = Rational 0 1         -- Standard form for 0
    | otherwise  = Rational n'' d''
  where
    g = gcd n d
    n' = n `quot` g
    d' = d `quot` g
    -- Ensure denominator is positive
    (n'', d'') = if d' < 0 then (-n', -d') else (n', d')

-- Constructor function ---------------------------------------------------------
rational :: Integral a => (a, a) -> Rational a
rational (n, d) = normalize n d

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = normalize (abs n) (abs d)

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = normalize (n1 * d2 + n2 * d1) (d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = normalize (n1 * d2 - n2 * d1) (d1 * d2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = normalize (n1 * n2) (d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2) = normalize (n1 * d2) (n2 * d1)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) e
    | e >= 0    = normalize (n ^ e) (d ^ e)
    | n == 0    = error "Division by zero in pow" -- 0 to a negative power is undefined
    | otherwise  = normalize (d ^ (-e)) (n ^ (-e))

expRational :: (Integral a, Floating b) => Rational a -> b -> b
expRational (Rational n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: (Floating a, Integral b) => a -> Rational b -> a
expReal x (Rational n d) = x ** (fromIntegral n / fromIntegral d)
