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

import Data.Time.Calendar
  ( Day
  , toGregorian
  , fromGregorian
  , fromGregorianValid
  , gregorianMonthLength
  )

--------------------------------------------------------------------------------
-- | Data definitions
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- | Task functions
--------------------------------------------------------------------------------

-- | Extract the street name of the place where the person was born.
bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

-- | Change the street of the person's current address.
setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person =
  person
    { _address = (_address person) { _street = street } }

-- | Update only the month component of the birthday, keeping the
--   original year and day-of-month. If the resulting day is invalid
--   for the new month (e.g. 31st of February), it is clamped to the
--   last valid day of that month.
setBirthMonth :: Int        -- ^ New month (1-12)
              -> Person
              -> Person
setBirthMonth month person =
  let oldBorn          = _born person
      (y, _m, d)       = toGregorian (_bornOn oldBorn)
      newDayValidated  = case fromGregorianValid y month d of
                           Just day -> day
                           Nothing  -> fromGregorian y month (gregorianMonthLength y month)
      newBorn          = oldBorn { _bornOn = newDayValidated }
  in person { _born = newBorn }

-- | Apply a function to both the current street and the birth street.
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let -- Update current address
      currentAddr      = _address person
      currentAddr'     = currentAddr { _street = f (_street currentAddr) }

      -- Update birth address
      bornInfo         = _born person
      bornAddr         = _bornAt bornInfo
      bornAddr'        = bornAddr { _street = f (_street bornAddr) }
      bornInfo'        = bornInfo { _bornAt = bornAddr' }
  in person { _address = currentAddr', _born = bornInfo' }
