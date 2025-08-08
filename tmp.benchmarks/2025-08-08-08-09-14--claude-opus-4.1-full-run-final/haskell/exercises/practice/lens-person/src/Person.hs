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

data Person = Person { name    :: Name
                     , born    :: Born
                     , address :: Address
                     }

data Name = Name { foreNames :: String
                 , surName   :: String
                 }

data Born = Born { bornAt :: Address
                 , bornOn :: Day
                 }

data Address = Address { street      :: String
                       , houseNumber :: Int
                       , place       :: String
                       , country     :: String
                       }

bornStreet :: Born -> String
bornStreet b = street (bornAt b)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet person = 
  person { address = (address person) { street = newStreet } }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = 
  person { born = (born person) { bornOn = newDay } }
  where
    oldDay = bornOn (born person)
    (year, _, dayOfMonth) = toGregorian oldDay
    newDay = fromGregorian year month dayOfMonth

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = 
  person { address = updatedAddress
         , born = updatedBorn
         }
  where
    updatedAddress = (address person) { street = f (street (address person)) }
    updatedBorn = (born person) { bornAt = updatedBornAddress }
    updatedBornAddress = (bornAt (born person)) { street = f (street (bornAt (born person))) }
