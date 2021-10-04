package Matrix;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => row column';
use List::Util  qw/ reduce /;

sub matrix {
    my ($text) = @_;

    ## no critic (RegularExpressions::RequireExtendedFormatting)
    my @rows = map { [split] } split /\n/, $text;

    # assume the matrix is square
    my $cols = reduce {$a->[$b] = [map {$_->[$b]} @rows]; $a}
                      [],
                      0 .. $rows[0]->$#*;

    return {rows => \@rows, columns => $cols};
}

sub row {
    my ($index, $string) = (shift)->@{'index', 'string'};
    return matrix($string)->{rows}[$index - 1];
}

sub column {
    my ($index, $string) = (shift)->@{'index', 'string'};
    return matrix($string)->{columns}[$index - 1];
}

1;
