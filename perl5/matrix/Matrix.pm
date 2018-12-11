package Matrix;
use strict;
use warnings;
use 5.024;
use List::Util  qw/ reduce /;
## no critic (RegularExpressions::RequireExtendedFormatting)

sub new {
    my ($class, $text) = @_;
    my @rows = map { [split] } split /\n/, $text;

    # assume the matrix is square
    my $cols = reduce {$a->[$b] = [map {$_->[$b]} @rows]; $a} [], 0 .. $rows[0]->$#*;

    return bless [\@rows, $cols], $class;
}

sub rows    { return shift->[0]->[pop] }
sub columns { return shift->[1]->[pop] }

1;
