{-# LANGUAGE RecordWildCards #-}

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

-- | Data describing a person and related items
data Person = Person
  { _name    :: Name
  , _born    :: Born
  , _address :: Address
  } deriving (Eq, Show)

data Name = Name
  { _foreNames :: String
  , _surName   :: String
  } deriving (Eq, Show)

data Born = Born
  { _bornAt :: Address
  , _bornOn :: Day
  } deriving (Eq, Show)

data Address = Address
  { _street      :: String
  , _houseNumber :: Int
  , _place       :: String
  , _country     :: String
  } deriving (Eq, Show)

-- | Obtain the street on which someone was born
bornStreet :: Born -> String
bornStreet Born{ _bornAt = Address{ _street = s } } = s

-- | Change the current street of a person
setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet person@Person{ _address = addr@Address{..} } =
  person { _address = addr { _street = newStreet } }

-- | Change the month component of a person's birthday, keeping year and day
setBirthMonth :: Int -> Person -> Person
setBirthMonth newMonth person@Person{ _born = b@Born{ _bornOn = day } } =
  let (year, _oldMonth, monthDay) = toGregorian day
      newDay                       = fromGregorian year newMonth monthDay
  in  person { _born = b { _bornOn = newDay } }

-- | Apply a transformation function to every street associated with the person
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person@Person{ _address = addr@Address{ _street = curStreet }
                             , _born    = b@Born{ _bornAt = bAddr@Address{ _street = bornStreet' } }
                             } =
  let addr'  = addr { _street  = f curStreet }
      bAddr' = bAddr { _street = f bornStreet' }
      born'  = b { _bornAt = bAddr' }
  in  person { _address = addr', _born = born' }
