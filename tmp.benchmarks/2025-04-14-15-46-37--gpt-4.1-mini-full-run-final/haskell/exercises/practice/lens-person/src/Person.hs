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

import Data.Time.Calendar (Day)
import Data.Time.Calendar (toGregorian, fromGregorian)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))

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
name :: Functor f => (Name -> f Name) -> Person -> f Person
name f p = fmap (\n -> p { _name = n }) (f (_name p))

born :: Functor f => (Born -> f Born) -> Person -> f Person
born f p = fmap (\b -> p { _born = b }) (f (_born p))

address :: Functor f => (Address -> f Address) -> Person -> f Person
address f p = fmap (\a -> p { _address = a }) (f (_address p))

-- Lenses for Born
bornAt :: Functor f => (Address -> f Address) -> Born -> f Born
bornAt f b = fmap (\a -> b { _bornAt = a }) (f (_bornAt b))

bornOn :: Functor f => (Day -> f Day) -> Born -> f Born
bornOn f b = fmap (\d -> b { _bornOn = d }) (f (_bornOn b))

-- Lenses for Address
street :: Functor f => (String -> f String) -> Address -> f Address
street f a = fmap (\s -> a { _street = s }) (f (_street a))

houseNumber :: Functor f => (Int -> f Int) -> Address -> f Address
houseNumber f a = fmap (\n -> a { _houseNumber = n }) (f (_houseNumber a))

place :: Functor f => (String -> f String) -> Address -> f Address
place f a = fmap (\p -> a { _place = p }) (f (_place a))

country :: Functor f => (String -> f String) -> Address -> f Address
country f a = fmap (\c -> a { _country = c }) (f (_country a))

-- Helper function to use lenses for setting a value
set :: ((a -> Identity a) -> s -> Identity s) -> a -> s -> s
set lens val s = runIdentity (lens (\_ -> Identity val) s)

-- Helper function to use lenses for getting a value
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view lens s = getConst (lens Const s)

-- Helper function to modify a value using a lens
over :: ((a -> Identity a) -> s -> Identity s) -> (a -> a) -> s -> s
over lens f s = runIdentity (lens (Identity . f) s)

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet = over address (set street newStreet)

setBirthMonth :: Int -> Person -> Person
setBirthMonth newMonth = over born $ \b ->
  let (y, _, d) = toGregorian (_bornOn b)
      newDay = fromGregorian y newMonth d
  in b { _bornOn = newDay }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over address (over street f) . over born (over bornAt (over street f))
