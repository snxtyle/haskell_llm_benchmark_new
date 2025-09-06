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

import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving(P.Eq, P.Show)

rational :: P.Integral a => (a, a) -> Rational a
rational (a, b) = if b P.== 0 then P.error "Denominator cannot be zero" else reduce (Rational a b)
  where
    reduce (Rational n d) = let g = P.gcd n d
                                n' = n P.`div` g
                                d' = d P.`div` g
                            in if d' P.< 0 then Rational (P.negate n') (P.negate d') else Rational n' d'

-- unary operators -------------------------------------------------------------
abs :: P.Integral a => Rational a -> Rational a
abs r@(Rational n d) = if n P.< 0 then Rational (P.negate n) d else r

numerator :: P.Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: P.Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: P.Integral a => Rational a -> Rational a -> Rational a
add (Rational n1 d1) (Rational n2 d2) = rational (n1 P.* d2 P.+ n2 P.* d1, d1 P.* d2)

sub :: P.Integral a => Rational a -> Rational a -> Rational a
sub (Rational n1 d1) (Rational n2 d2) = rational (n1 P.* d2 P.- n2 P.* d1, d1 P.* d2)

mul :: P.Integral a => Rational a -> Rational a -> Rational a
mul (Rational n1 d1) (Rational n2 d2) = rational (n1 P.* n2, d1 P.* d2)

div :: P.Integral a => Rational a -> Rational a -> Rational a
div (Rational n1 d1) (Rational n2 d2) = if n2 P.== 0 then P.error "Division by zero" else rational (n1 P.* d2, n2 P.* d1)

pow :: P.Integral a => Rational a -> a -> Rational a
pow (Rational n1 d1) n
  | n P.>= 0 = rational (n1 P.^ n, d1 P.^ n)
  | n1 P.== 0 = P.error "Zero to negative power"
  | P.otherwise = rational (d1 P.^ (P.negate n), n1 P.^ (P.negate n))

expRational :: P.Integral a => P.Floating b => Rational a -> b -> b
expRational (Rational n1 d1) x = P.fromIntegral n1 P.** x P./ P.fromIntegral d1 P.** x

expReal :: P.Floating a => P.Integral b => a -> Rational b -> a
expReal x (Rational n d) = x P.** (P.fromIntegral n P./ P.fromIntegral d)
