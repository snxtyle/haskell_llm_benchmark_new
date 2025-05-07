module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

-- Earth year in seconds, as per the problem description.
earthYearInSeconds :: Float
earthYearInSeconds = 31557600.0

-- Helper function to get the orbital period of a planet in Earth years.
orbitalPeriod :: Planet -> Float
orbitalPeriod Mercury = 0.2408467
orbitalPeriod Venus   = 0.61519726
orbitalPeriod Earth   = 1.0
orbitalPeriod Mars    = 1.8808158
orbitalPeriod Jupiter = 11.862615
orbitalPeriod Saturn  = 29.447498
orbitalPeriod Uranus  = 84.016846
orbitalPeriod Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
    let ageInEarthYears = seconds / earthYearInSeconds
    in ageInEarthYears / orbitalPeriod planet
