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
setCurrentStreet street person = person 
  { _address = (_address person) { _street = street } }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = 
  let born' = _born person
      (y, _, d) = toGregorian $ _bornOn born'
      newDay = fromGregorian y month d
  in person { _born = born' { _bornOn = newDay } }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = 
  let updateAddr addr = addr { _street = f (_street addr) }
      newAddress = updateAddr (_address person)
      bornAddr = updateAddr (_bornAt (_born person))
      newBorn = (_born person) { _bornAt = bornAddr }
  in person 
      { _address = newAddress
      , _born = newBorn 
      }
