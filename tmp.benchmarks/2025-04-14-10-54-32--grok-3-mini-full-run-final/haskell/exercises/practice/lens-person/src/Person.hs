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

-- Implement bornStreet: Get the street from the nested Address in Born
bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

-- Implement setCurrentStreet: Update the street in the Address of Person
setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet person =
  person { _address = (_address person) { _street = newStreet } }

-- Implement setBirthMonth: Update the month in the bornOn Day of Born in Person
-- Note: This assumes the new month is valid; in practice, you might need to handle invalid dates.
setBirthMonth :: Int -> Person -> Person
setBirthMonth newMonth person =
  let born = _born person
      bornOn = _bornOn born
      (year, _, day) = toGregorian bornOn  -- Extract year and day
      newBornOn = fromGregorian year newMonth day  -- Create new Day
  in person { _born = born { _bornOn = newBornOn } }

-- Implement renameStreets: Apply a function to all streets in Person
-- This updates the street in both _address and _born._bornAt
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let updatedAddress = (_address person) { _street = f (_street (_address person)) }
      updatedBornAt = (_bornAt (_born person)) { _street = f (_street (_bornAt (_born person))) }
      updatedBorn = (_born person) { _bornAt = updatedBornAt }
  in person { _born = updatedBorn, _address = updatedAddress }
