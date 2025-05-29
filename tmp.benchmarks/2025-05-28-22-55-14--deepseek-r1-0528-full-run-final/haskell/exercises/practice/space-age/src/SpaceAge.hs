module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYearInSeconds :: Float
earthYearInSeconds = 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = 
    case planet of
        Mercury -> seconds / (earthYearInSeconds * 0.2408467)
        Venus   -> seconds / (earthYearInSeconds * 0.61519726)
        Earth   -> seconds / earthYearInSeconds
        Mars    -> seconds / (earthYearInSeconds * 1.8808158)
        Jupiter -> seconds / (earthYearInSeconds * 11.862615)
        Saturn  -> seconds / (earthYearInSeconds * 29.447498)
        Uranus  -> seconds / (earthYearInSeconds * 84.016846)
        Neptune -> seconds / (earthYearInSeconds * 164.79132)
