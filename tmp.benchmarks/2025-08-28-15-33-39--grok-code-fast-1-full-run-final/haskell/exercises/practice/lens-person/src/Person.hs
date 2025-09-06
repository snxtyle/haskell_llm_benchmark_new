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

import Data.Time.Calendar (Day, toGregorian, fromGregorian)
import Data.Functor.Const
import Data.Functor.Identity

data Person = Person { _name :: Name, _born :: Born, _address :: Address }

data Name = Name { _foreNames :: String, _surName :: String }

data Born = Born { _bornAt :: Address, _bornOn :: Day }

data Address = Address { _street :: String, _houseNumber :: Int, _place :: String, _country :: String }

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

view :: Lens s a -> s -> a
view l s = getConst (l Const s)

set :: Lens s a -> a -> s -> s
set l a s = runIdentity (l (const (Identity a)) s)

over :: Lens s a -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

nameL :: Lens Person Name
nameL f p = fmap (\n -> p { _name = n }) (f (_name p))

bornL :: Lens Person Born
bornL f p = fmap (\b -> p { _born = b }) (f (_born p))

addressL :: Lens Person Address
addressL f p = fmap (\a -> p { _address = a }) (f (_address p))

foreNamesL :: Lens Name String
foreNamesL f n = fmap (\fns -> n { _foreNames = fns }) (f (_foreNames n))

surNameL :: Lens Name String
surNameL f n = fmap (\sn -> n { _surName = sn }) (f (_surName n))

bornAtL :: Lens Born Address
bornAtL f b = fmap (\a -> b { _bornAt = a }) (f (_bornAt b))

bornOnL :: Lens Born Day
bornOnL f b = fmap (\d -> b { _bornOn = d }) (f (_bornOn b))

streetL :: Lens Address String
streetL f a = fmap (\s -> a { _street = s }) (f (_street a))

houseNumberL :: Lens Address Int
houseNumberL f a = fmap (\hn -> a { _houseNumber = hn }) (f (_houseNumber a))

placeL :: Lens Address String
placeL f a = fmap (\p -> a { _place = p }) (f (_place a))

countryL :: Lens Address String
countryL f a = fmap (\c -> a { _country = c }) (f (_country a))

setMonth :: Int -> Day -> Day
setMonth m d = let (y, _, d') = toGregorian d in fromGregorian y m d'

bornStreet :: Born -> String
bornStreet born = view (bornAtL . streetL) born

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = set (addressL . streetL) street person

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = over (bornL . bornOnL) (setMonth month) person

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = over (bornL . bornAtL . streetL) f $ over (addressL . streetL) f person
