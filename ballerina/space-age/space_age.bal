const float SECONDS_PER_EARTH_YEAR = 31557600.0;

function age(string planet, int seconds) returns float|error {
    var planetAge = function(float period) returns float {
        float age = <float>seconds / period / SECONDS_PER_EARTH_YEAR;
        return age.round(2);
    };

    match planet {
        "Mercury" => { return planetAge(0.2408467); }
        "Venus"   => { return planetAge(0.61519726); }
        "Earth"   => { return planetAge(1.0); }
        "Mars"    => { return planetAge(1.8808158); }
        "Jupiter" => { return planetAge(11.862615); }
        "Saturn"  => { return planetAge(29.447498); }
        "Uranus"  => { return planetAge(84.016846); }
        "Neptune" => { return planetAge(164.79132); }
        _         => { return error("not a planet"); }
    }
}
