module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

secondsPerYear :: Planet -> Float
secondsPerYear planet = 
    secondsPerEarthYear * case planet of
        Mercury -> 0.2408467
        Venus   -> 0.61519726
        Earth   -> 1
        Mars    -> 1.8808158
        Jupiter -> 11.862615
        Saturn  -> 29.447498
        Uranus  -> 84.016846
        Neptune -> 164.79132
    where
        secondsPerEarthYear = 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / secondsPerYear planet
