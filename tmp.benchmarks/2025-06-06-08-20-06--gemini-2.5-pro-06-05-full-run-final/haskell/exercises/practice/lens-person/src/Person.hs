{-# LANGUAGE TemplateHaskell #-}
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

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }
makeLenses ''Person

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }
makeLenses ''Name

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }
makeLenses ''Born

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }
makeLenses ''Address

-- A custom lens for the month of a Day.
-- It knows how to get and set the month field of a Day,
-- which doesn't exist as a record field.
month :: Lens' Day Int
month = lens getter setter
  where
    getter day = let (_, m, _) = toGregorian day in m
    setter day newMonth = let (y, _, d) = toGregorian day in fromGregorian y newMonth d

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set (born . bornOn . month)

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & address . street %~ f
                                & born . bornAt . street %~ f
