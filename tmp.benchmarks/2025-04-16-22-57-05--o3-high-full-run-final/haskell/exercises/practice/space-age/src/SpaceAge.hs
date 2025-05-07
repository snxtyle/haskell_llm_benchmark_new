module SpaceAge (Planet(..), ageOn) where

-- | A data type representing each planet in our solar system.
--   Deriving the common typeclasses makes testing and debugging easier.
data Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    deriving (Eq, Show, Read)

-- | Number of seconds in an Earth year (per the exercise spec).
earthYearSeconds :: Float
earthYearSeconds = 31557600

-- | Orbital period of each planet expressed in Earth years.
orbitalPeriod :: Planet -> Float
orbitalPeriod Mercury = 0.2408467
orbitalPeriod Venus   = 0.61519726
orbitalPeriod Earth   = 1.0
orbitalPeriod Mars    = 1.8808158
orbitalPeriod Jupiter = 11.862615
orbitalPeriod Saturn  = 29.447498
orbitalPeriod Uranus  = 84.016846
orbitalPeriod Neptune = 164.79132

-- | Calculate a person's age on a given planet from their age in seconds.
ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (earthYearSeconds * orbitalPeriod planet)
