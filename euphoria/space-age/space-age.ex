public function ageOn(sequence planet, integer seconds) 
    atom relOrbit = earthYearsPerOrbit(planet)
    if relOrbit = NOT_A_PLANET then
        return FALSE
    else
        return seconds / SECONDS_PER_EARTH_YEAR / relOrbit
    end if
end function

constant SECONDS_PER_EARTH_YEAR = 31557600
constant NOT_A_PLANET = -1
constant FALSE = 0

function earthYearsPerOrbit(sequence planet)
    switch planet do
        case "Mercury" then return   0.2408467
        case "Venus"   then return   0.61519726
        case "Earth"   then return   1.0
        case "Mars"    then return   1.8808158
        case "Jupiter" then return  11.862615
        case "Saturn"  then return  29.447498
        case "Uranus"  then return  84.016846
        case "Neptune" then return 164.79132
        case else           return NOT_A_PLANET
    end switch
end function
