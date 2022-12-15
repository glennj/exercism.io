package Matrix;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => row column';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ row column /;

use List::Util  qw/ reduce /;

use Class::Tiny qw/ rows cols /;
sub BUILDARGS {
    my ($class, %args) = @_;

    ## no critic (RegularExpressions::RequireExtendedFormatting)
    my @rows = map { [split] } split /\n/, $args{'string'};

    # assume the matrix is square
    my $cols = reduce {$a->[$b] = [map {$_->[$b]} @rows]; $a}
                      [],
                      0 .. $rows[0]->$#*;

    return {rows => \@rows, cols => $cols};
}

sub row {
    my ($self, $index) = @_;
    return $self->rows->[$index - 1];
}

sub column {
    my ($self, $index) = @_;
    return $self->cols->[$index - 1];
}

1;
