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

import Data.Time.Calendar (Day, fromGregorian, toGregorian)

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

-- Simple lens type
type Lens s a = (a -> a) -> s -> s

-- Lenses for Person
personBorn :: Lens Person Born
personBorn f person = person { _born = f (_born person) }

personAddress :: Lens Person Address
personAddress f person = person { _address = f (_address person) }

-- Lenses for Born
bornAtLens :: Lens Born Address
bornAtLens f born = born { _bornAt = f (_bornAt born) }

bornOnLens :: Lens Born Day
bornOnLens f born = born { _bornOn = f (_bornOn born) }

-- Lens for Address
addressStreet :: Lens Address String
addressStreet f address = address { _street = f (_street address) }

-- Lens composition
compose :: Lens s a -> Lens a b -> Lens s b
compose l1 l2 f = l1 (l2 f)

-- Get the street where the person was born
bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

-- Set the current street of a person
setCurrentStreet :: String -> Person -> Person
setCurrentStreet street = personAddress (addressStreet (const street))

-- Set the birth month of a person
setBirthMonth :: Int -> Person -> Person
setBirthMonth month = personBorn (bornOnLens updateMonth)
  where
    updateMonth day =
      let (year, _, dayOfMonth) = toGregorian day
      in fromGregorian year month dayOfMonth

-- Rename all streets (both current and birth address)
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let person' = personAddress (addressStreet f) person
  in personBorn (bornAtLens (addressStreet f)) person'
