module SpaceAge (Planet(..), ageOn) where

-- | Enumeration of the planets that we care about for this exercise.
data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
            deriving (Eq, Show)

-- | Number of seconds in one Earth year, as defined by the exercise.
earthYearSeconds :: Float
earthYearSeconds = 31557600.0

-- | Orbital period of each planet, expressed in Earth years.
orbitalPeriod :: Planet -> Float
orbitalPeriod planet = case planet of
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Earth   -> 1.0
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132

-- | Calculate how old someone would be on a given planet
--   given their age in seconds.
--
--   The algorithm is:
--     1. Convert the input seconds to Earth years.
--     2. Divide that value by the planet's orbital period
--        (in Earth years) to get the age on that planet.
ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthYears / orbitalPeriod planet
  where
    earthYears = seconds / earthYearSeconds
