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
  deriving (Show, Eq)

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }
  deriving (Show, Eq)

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }
  deriving (Show, Eq)

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }
  deriving (Show, Eq)

-- | Get the street name of the address where the person was born.
bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

-- | Set the street name of the person's current address.
setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person =
  let currentAddr = _address person
      newAddr = currentAddr { _street = street }
  in person { _address = newAddr }

-- Helper function to set the month of a Day
setDayMonth :: Int -> Day -> Day
setDayMonth month day = let (y, _, d) = toGregorian day in fromGregorian y month d

-- | Set the month of the person's birth date.
setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  let bornRec = _born person
      bornDay = _bornOn bornRec
      newBornDay = setDayMonth month bornDay
      newBornRec = bornRec { _bornOn = newBornDay }
  in person { _born = newBornRec }

-- | Apply a function to rename streets in both current and birth addresses.
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let currentAddr = _address person
      bornRec     = _born person
      bornAddr    = _bornAt bornRec

      newCurrentAddr = currentAddr { _street = f (_street currentAddr) }
      newBornAddr    = bornAddr { _street = f (_street bornAddr) }

      newBornRec = bornRec { _bornAt = newBornAddr }
  in person { _address = newCurrentAddr, _born = newBornRec }

