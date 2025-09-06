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
setCurrentStreet street person = 
  person { _address = (_address person) { _street = street } }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = 
  let born = _born person
      day = _bornOn born
      (year, _, dayOfMonth) = toGregorian day
      newDay = fromGregorian year month dayOfMonth
  in person { _born = born { _bornOn = newDay } }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = 
  person { 
    _address = (_address person) { _street = f (_street (_address person)) },
    _born = (_born person) { 
      _bornAt = (_bornAt (_born person)) { 
        _street = f (_street (_bornAt (_born person))) 
      } 
    }
  }
