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

import Control.Lens
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Language.Haskell.TH

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

-- Generate lenses for all data types
makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

-- | Get the street name where a person was born.
bornStreet :: Born -> String
bornStreet = view (bornAt . street)

-- | Set the street name of a person's current address.
setCurrentStreet :: String -> Person -> Person
setCurrentStreet street = set (address . street) street

-- | Set the month of a person's birth date.
setBirthMonth :: Int -> Person -> Person
setBirthMonth month = over (born . bornOn) updateMonth
  where
    updateMonth :: Day -> Day
    updateMonth day =
      let (year, _, dayOfMonth) = toGregorian day
      in fromGregorian year month dayOfMonth

-- | Apply a function to rename both the current street and the birth street.
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & address . street %~ f
                                & born . bornAt . street %~ f
