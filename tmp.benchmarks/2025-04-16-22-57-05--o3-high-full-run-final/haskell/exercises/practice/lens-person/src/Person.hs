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

-- | Extract the street name of the birth address.
bornStreet :: Born -> String
bornStreet = _street . _bornAt

-- | Update the street of the current address of a person.
setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person =
  person { _address = (_address person) { _street = street } }

-- | Change the month of birth, keeping the original year and day‑of‑month.
setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  let bornInfo = _born person
      (y, _oldM, d) = toGregorian (_bornOn bornInfo)
      newDay       = fromGregorian y month d
      newBorn      = bornInfo { _bornOn = newDay }
  in person { _born = newBorn }

-- | Apply a transformation function to every street contained in the person
-- (both the current address and the birth address).
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let addr          = _address person
      newAddr       = addr { _street = f (_street addr) }

      bornInfo      = _born person
      bornAddr      = _bornAt bornInfo
      newBornAddr   = bornAddr { _street = f (_street bornAddr) }
      newBornInfo   = bornInfo { _bornAt = newBornAddr }
  in person { _address = newAddr, _born = newBornInfo }
