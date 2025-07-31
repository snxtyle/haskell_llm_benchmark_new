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
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = person & born . bornOn %~ (updateMonth month)
  where
    updateMonth m d = let (y, _, d') = toGregorian d in fromGregorian y m d'

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & (address . street %~ f) & (born . bornAt . street %~ f)
