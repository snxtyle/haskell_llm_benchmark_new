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

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Control.Lens

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     } deriving (Show, Eq)

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 } deriving (Show, Eq)

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 } deriving (Show, Eq)

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       } deriving (Show, Eq)

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet = set (address . street) newStreet

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = 
  let currentDay = view (born . bornOn) person
      (year, _, day) = toGregorian currentDay
      newDay = fromGregorian year month day
  in set (born . bornOn) newDay person

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = 
  let modifyStreet = over street f
      modifyBornAt = over bornAt modifyStreet
      modifyAddress = over address modifyStreet
  in person & born .~ modifyBornAt (view born person)
            & address .~ modifyAddress (view address person)
