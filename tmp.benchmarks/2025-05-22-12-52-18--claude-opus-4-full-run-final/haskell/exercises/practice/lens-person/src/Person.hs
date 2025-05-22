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
    let (year, _, day) = toGregorian (_bornOn (_born person))
        newBornOn = fromGregorian year month day
        newBorn = (_born person) { _bornOn = newBornOn }
    in person { _born = newBorn }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = 
    let currentAddress = _address person
        newCurrentAddress = currentAddress { _street = f (_street currentAddress) }
        
        birthAddress = _bornAt (_born person)
        newBirthAddress = birthAddress { _street = f (_street birthAddress) }
        newBorn = (_born person) { _bornAt = newBirthAddress }
    in person { _address = newCurrentAddress, _born = newBorn }
