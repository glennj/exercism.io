package SpaceAge;

use v5.38;
use Carp;

use Exporter qw/ import /;
our @EXPORT_OK = qw/ age_on_planet /;

our $SEC_PER_EARTH_YEAR = 31_557_600;
our %RELATIVE_YEARS = (
    mercury =>   0.2408467,
    venus   =>   0.61519726,
    earth   =>   1.0,
    mars    =>   1.8808158,
    jupiter =>  11.862615,
    saturn  =>  29.447498,
    uranus  =>  84.016846,
    neptune => 164.79132,
);

sub age_on_planet($planet, $seconds) {
    croak "not a planet" unless exists $RELATIVE_YEARS{lc $planet};

    return sprintf "%.2f", $seconds / $SEC_PER_EARTH_YEAR 
                                    / $RELATIVE_YEARS{lc $planet};
}

1;
