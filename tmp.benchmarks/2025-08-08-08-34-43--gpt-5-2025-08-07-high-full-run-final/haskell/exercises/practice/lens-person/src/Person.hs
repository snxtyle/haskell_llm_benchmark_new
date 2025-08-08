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

import Data.Time.Calendar (Day, toGregorian, fromGregorian, gregorianMonthLength)

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

-- Minimal lens machinery (no external packages)
data Lens s a = Lens (s -> a) (a -> s -> s)

view :: Lens s a -> s -> a
view (Lens get _) = get

set :: Lens s a -> a -> s -> s
set (Lens _ put) = put

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = set ln (f (view ln s)) s

infixr 9 |.|
(|.|) :: Lens s a -> Lens a b -> Lens s b
(Lens get1 set1) |.| (Lens get2 set2) =
  Lens
    (get2 . get1)
    (\b s -> let a  = get1 s
                 a' = set2 b a
             in set1 a' s)

-- Field lenses needed for the tasks
bornAtL :: Lens Born Address
bornAtL = Lens _bornAt (\addr b -> b { _bornAt = addr })

bornOnL :: Lens Born Day
bornOnL = Lens _bornOn (\day b -> b { _bornOn = day })

addressL :: Lens Person Address
addressL = Lens _address (\addr p -> p { _address = addr })

streetL :: Lens Address String
streetL = Lens _street (\str a -> a { _street = str })

bornL :: Lens Person Born
bornL = Lens _born (\b p -> p { _born = b })

bornStreet :: Born -> String
bornStreet = view (bornAtL |.| streetL)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet = set (addressL |.| streetL) newStreet

setBirthMonth :: Int -> Person -> Person
setBirthMonth month =
  over (bornL |.| bornOnL) updateMonth
  where
    updateMonth :: Day -> Day
    updateMonth d =
      let (y, _, dayOfMonth) = toGregorian d
          maxDay = gregorianMonthLength y month
      in fromGregorian y month (min dayOfMonth maxDay)

renameStreets :: (String -> String) -> Person -> Person
renameStreets f =
  over (addressL |.| streetL) f .
  over (bornL |.| bornAtL |.| streetL) f
