package SpaceAge;
use strictures 2;

our $SEC_PER_EARTH_YEAR = 31_557_600;
our %RELATIVE_YEARS = (
    mercury => 0.2408467,
    venus   => 0.61519726,
    earth   => 1,
    mars    => 1.8808158,
    jupiter => 11.862615,
    saturn  => 29.447498,
    uranus  => 84.016846,
    neptune => 164.79132,
);
our $AUTOLOAD;

use Class::Tiny 'seconds';
sub BUILDARGS { return {seconds => pop} }

# Inject the "on_" function for each planet. Thanks Yanick.
for my $planet (keys %RELATIVE_YEARS) {
    my $func = "on_$planet";
    no strict;
    *$func = sub {
        my $self = shift;
        return sprintf "%.2f", $self->seconds / $SEC_PER_EARTH_YEAR / $RELATIVE_YEARS{$planet};
    }
}

1;
