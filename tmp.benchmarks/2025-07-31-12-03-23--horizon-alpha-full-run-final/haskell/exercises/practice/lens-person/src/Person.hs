{-# LANGUAGE RankNTypes #-}
module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day, toGregorian, fromGregorianValid, gregorianMonthLength)

-- A very small lens implementation (no external packages).
-- Lens type: a function that focuses on a part 'a' inside 's' and can update it in a functorial way.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- View
infixl 8 ^.
(^.) :: s -> Lens' s a -> a
s ^. l = getConst (l Const s)

-- Set
infixr 4 .~
(.~) :: Lens' s a -> a -> s -> s
l .~ b = over l (const b)

-- Over/modify
infixr 4 %~
(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

over :: Lens' s a -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

-- Lens composition (to avoid clashing with Prelude (.))
infixr 9 .@
(.@) :: Lens' a b -> Lens' b c -> Lens' a c
(l1 .@ l2) f s = l1 (\b -> l2 f b) s

-- Field lenses for our records

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

-- Lenses for Person
nameL :: Lens' Person Name
nameL f (Person n b a) = fmap (\n' -> Person n' b a) (f n)

bornL :: Lens' Person Born
bornL f (Person n b a) = fmap (\b' -> Person n b' a) (f b)

addressL :: Lens' Person Address
addressL f (Person n b a) = fmap (\a' -> Person n b a') (f a)

-- Lenses for Name
foreNamesL :: Lens' Name String
foreNamesL f (Name fn sn) = fmap (\fn' -> Name fn' sn) (f fn)

surNameL :: Lens' Name String
surNameL f (Name fn sn) = fmap (\sn' -> Name fn sn') (f sn)

-- Lenses for Born
bornAtL :: Lens' Born Address
bornAtL f (Born at on) = fmap (\at' -> Born at' on) (f at)

bornOnL :: Lens' Born Day
bornOnL f (Born at on) = fmap (\on' -> Born at on') (f on)

-- Lenses for Address
streetL :: Lens' Address String
streetL f (Address s h p c) = fmap (\s' -> Address s' h p c) (f s)

houseNumberL :: Lens' Address Int
houseNumberL f (Address s h p c) = fmap (\h' -> Address s h' p c) (f h)

placeL :: Lens' Address String
placeL f (Address s h p c) = fmap (\p' -> Address s h p' c) (f p)

countryL :: Lens' Address String
countryL f (Address s h p c) = fmap (\c' -> Address s h p c') (f c)

-- Utilities for lens internals
newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where
  fmap g (Identity x) = Identity (g x)

newtype Const a b = Const { getConst :: a }
instance Functor (Const a) where
  fmap _ (Const x) = Const x

bornStreet :: Born -> String
bornStreet born = born ^. bornAtL ^. streetL

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street = (addressL .@ streetL) .~ street

setBirthMonth :: Int -> Person -> Person
setBirthMonth month =
  let clampMonth m = max 1 (min 12 m)
      setMonth day =
        let (y, _m, d) = toGregorian day
            m' = clampMonth month
            maxD = gregorianMonthLength y m'
            d' = min d maxD
        in  maybe day id (fromGregorianValid y m' d')
              -- We clamp to ensure validity; fallback to original day if ever Nothing.
  in (bornL .@ bornOnL) %~ \day ->
       let (y, _m, d) = toGregorian day
           m' = clampMonth month
           maxD = gregorianMonthLength y m'
           d'' = min d maxD
       in maybe day id (fromGregorianValid y m' d'')

renameStreets :: (String -> String) -> Person -> Person
renameStreets f =
  -- Update both current address street and birth address street
  (addressL .@ streetL %~ f) . (bornL .@ bornAtL .@ streetL %~ f)
