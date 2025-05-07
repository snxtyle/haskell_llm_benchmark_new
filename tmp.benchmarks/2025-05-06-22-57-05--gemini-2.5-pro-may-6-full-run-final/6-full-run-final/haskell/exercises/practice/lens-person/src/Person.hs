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

bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet person = person { _address = updatedAddress }
  where
    currentAddress = _address person
    updatedAddress = currentAddress { _street = newStreet }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = person { _born = updatedBorn }
  where
    bornRecord = _born person
    currentDay = _bornOn bornRecord
    (year, _, dayOfMonth) = toGregorian currentDay
    newDay = fromGregorian year month dayOfMonth
    updatedBorn = bornRecord { _bornOn = newDay }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person { _address = updatedCurrentAddress, _born = updatedBornRecord }
  where
    -- Update street in current address
    currentAddr = _address person
    updatedCurrentAddress = currentAddr { _street = f (_street currentAddr) }

    -- Update street in bornAt address
    bornRec = _born person
    bornAddr = _bornAt bornRec
    updatedBornAddress = bornAddr { _street = f (_street bornAddr) }
    updatedBornRecord = bornRec { _bornAt = updatedBornAddress }
