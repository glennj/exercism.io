#!/usr/bin/env gawk -f
@include "assert"

BEGIN {
    earthYearsPerOrbit["Mercury"] =   0.2408467
    earthYearsPerOrbit["Venus"]   =   0.61519726
    earthYearsPerOrbit["Earth"]   =   1.0
    earthYearsPerOrbit["Mars"]    =   1.8808158
    earthYearsPerOrbit["Jupiter"] =  11.862615
    earthYearsPerOrbit["Saturn"]  =  29.447498
    earthYearsPerOrbit["Uranus"]  =  84.016846
    earthYearsPerOrbit["Neptune"] = 164.79132

    secondsPerEarthYear = 31557600
}

function ageOnPlanet(ageInSeconds, planet) {
    return ageInSeconds / secondsPerEarthYear / earthYearsPerOrbit[planet]
}

{
    assert($1 in earthYearsPerOrbit, "not a planet")
    printf "%.2f\n", ageOnPlanet($2, $1)
}
