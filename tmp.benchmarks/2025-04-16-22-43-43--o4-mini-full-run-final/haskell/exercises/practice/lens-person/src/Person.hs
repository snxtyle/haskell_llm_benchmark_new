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

data Person = Person
  { _name    :: Name
  , _born    :: Born
  , _address :: Address
  }

data Name = Name
  { _foreNames :: String
  , _surName   :: String
  }

data Born = Born
  { _bornAt :: Address
  , _bornOn :: Day
  }

data Address = Address
  { _street      :: String
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
  let bornRec    = _born person
      origDay    = _bornOn bornRec
      (y, _, d)  = toGregorian origDay
      newDay     = fromGregorian y month d
      bornRec'   = bornRec { _bornOn = newDay }
  in person { _born = bornRec' }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let addr       = _address person
      addr'      = addr { _street = f (_street addr) }
      bornRec    = _born person
      bornAddr   = _bornAt bornRec
      bornAddr'  = bornAddr { _street = f (_street bornAddr) }
      bornRec'   = bornRec { _bornAt = bornAddr' }
  in person { _address = addr', _born = bornRec' }
