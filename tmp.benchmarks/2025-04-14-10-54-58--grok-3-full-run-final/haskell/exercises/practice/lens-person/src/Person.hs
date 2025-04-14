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
setCurrentStreet street person = person { _address = (_address person) { _street = street } }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = 
    let oldBorn = _born person
        oldDay = _bornOn oldBorn
        (year, _, day) = toGregorian oldDay
    in person { _born = oldBorn { _bornOn = fromGregorian year month day } }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = 
    let oldAddress = _address person
        oldBorn = _born person
        oldBornAt = _bornAt oldBorn
    in person { _address = oldAddress { _street = f (_street oldAddress) },
                _born = oldBorn { _bornAt = oldBornAt { _street = f (_street oldBornAt) } } }
