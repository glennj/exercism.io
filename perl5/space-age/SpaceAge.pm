package SpaceAge;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => age_on_planet';

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

sub age_on_planet {
    my ($planet, $seconds) = (shift)->@{'planet', 'seconds'};
    return sprintf "%.2f", $seconds / $SEC_PER_EARTH_YEAR 
                                    / $RELATIVE_YEARS{lc $planet};
}

1;
