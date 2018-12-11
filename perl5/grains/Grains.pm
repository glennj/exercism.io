package Grains;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(grains_on_square total_grains);
use Carp;
use List::Util  qw( sum );

sub grains_on_square {
    my $square = shift;
    croak 'ChessException' unless 1 <= $square and $square <= 64;
    return 2 ** ($square - 1);
}

sub total_grains {
    return sum map {grains_on_square $_} 1 .. 64;
}

1;
