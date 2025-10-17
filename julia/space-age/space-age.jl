const seconds_per_earth_year = 31_557_600

const earth_years_per_orbit = Base.ImmutableDict(
    "Mercury" =>   0.2408467,
    "Venus"   =>   0.61519726,
    "Earth"   =>   1.0,
    "Mars"    =>   1.8808158,
    "Jupiter" =>  11.862615,
    "Saturn"  =>  29.447498,
    "Uranus"  =>  84.016846,
    "Neptune" => 164.79132
)

for (planet, factor) in earth_years_per_orbit
    @eval function $(Symbol("on", planet))(seconds)
        seconds / seconds_per_earth_year / $factor
    end
end
