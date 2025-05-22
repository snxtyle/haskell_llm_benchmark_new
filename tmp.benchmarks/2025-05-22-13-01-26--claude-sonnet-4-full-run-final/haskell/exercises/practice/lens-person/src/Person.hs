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

-- Simple lens-like functions for accessing and updating nested records

-- Lens for Person's address
addressLens :: Person -> Address
addressLens = _address

setAddress :: Address -> Person -> Person
setAddress addr person = person { _address = addr }

-- Lens for Person's born
bornLens :: Person -> Born
bornLens = _born

setBorn :: Born -> Person -> Person
setBorn born person = person { _born = born }

-- Lens for Address's street
streetLens :: Address -> String
streetLens = _street

setStreet :: String -> Address -> Address
setStreet street addr = addr { _street = street }

-- Lens for Born's address
bornAtLens :: Born -> Address
bornAtLens = _bornAt

setBornAt :: Address -> Born -> Born
setBornAt addr born = born { _bornAt = addr }

-- Lens for Born's day
bornOnLens :: Born -> Day
bornOnLens = _bornOn

setBornOn :: Day -> Born -> Born
setBornOn day born = born { _bornOn = day }

-- Implementation of required functions

bornStreet :: Born -> String
bornStreet born = streetLens (bornAtLens born)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = 
  let currentAddr = addressLens person
      newAddr = setStreet street currentAddr
  in setAddress newAddr person

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  let born = bornLens person
      birthDay = bornOnLens born
      (year, _, day) = toGregorian birthDay
      newBirthDay = fromGregorian year month day
      newBorn = setBornOn newBirthDay born
  in setBorn newBorn person

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let -- Update current address street
      currentAddr = addressLens person
      newCurrentAddr = setStreet (f (streetLens currentAddr)) currentAddr
      
      -- Update birth address street
      born = bornLens person
      birthAddr = bornAtLens born
      newBirthAddr = setStreet (f (streetLens birthAddr)) birthAddr
      newBorn = setBornAt newBirthAddr born
      
      -- Combine both updates
      personWithNewCurrent = setAddress newCurrentAddr person
  in setBorn newBorn personWithNewCurrent
