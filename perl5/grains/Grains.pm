package Grains;

use strictures 2;
use Exporter::Easiest 'OK => grains_on_square total_grains';

use bignum;
use Carp;
use List::Util  qw( sum );


## no critic (ControlStructures::ProhibitNegativeExpressionsInUnlessAndUntilConditions)

sub grains_on_square {
    my $square = shift;
    croak 'square must be between 1 and 64'
        unless 1 <= $square and $square <= 64;
    return 2 ** ($square - 1);
}

sub total_grains {
    return sum map {grains_on_square $_} 1 .. 64;
}

1;
