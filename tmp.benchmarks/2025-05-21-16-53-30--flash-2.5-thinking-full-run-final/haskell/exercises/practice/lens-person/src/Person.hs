{-# LANGUAGE TemplateHaskell #-} -- Required for makeLenses

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  -- Exporting generated lenses for potential external use/testing
  , name, born, address
  , foreNames, surName
  , bornAt, bornOn
  , street, houseNumber, place, country
  ) where

import Data.Time.Calendar (Day, toGregorian, fromGregorian)
import Control.Lens       -- Import the lens library

-- Define Name, Born, and Address before Person
data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }
makeLenses ''Name -- Generates lenses like 'foreNames', 'surName'

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }
makeLenses ''Address -- Generates lenses like 'street', 'houseNumber', 'place', 'country'

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }
makeLenses ''Born -- Generates lenses like 'bornAt', 'bornOn'

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }
makeLenses ''Person -- Generates lenses like 'name', 'born', 'address'


-- Get the street from the born address
bornStreet :: Born -> String
bornStreet bornVal = bornVal ^. bornAt . street

-- Set the current street of a person's address
setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet person = person & address . street .~ newStreet

-- Set the birth month of a person
setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  person & born . bornOn %~ (\d -> let (y, _, day) = toGregorian d in fromGregorian y month day)

-- Rename all streets associated with a person (current address and born address)
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  person & (address . street <> born . bornAt . street) %~ f
