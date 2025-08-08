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

import Data.Time.Calendar (Day, toGregorian, fromGregorian, gregorianMonthLength)

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
  person { _born = oldBorn { _bornOn = newDay } }
  where
    oldBorn = _born person
    (year, _oldMonth, dayOfMonth) = toGregorian (_bornOn oldBorn)
    maxDay = gregorianMonthLength year month
    clampedDay = min dayOfMonth maxDay
    newDay = fromGregorian year month clampedDay

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  person
    { _address = currAddr { _street = f (_street currAddr) }
    , _born    = oldBorn  { _bornAt  = bornAddr { _street = f (_street bornAddr) } }
    }
  where
    currAddr = _address person
    oldBorn  = _born person
    bornAddr = _bornAt oldBorn
