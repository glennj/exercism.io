package Strain;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => keep discard';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ keep discard /;

sub keep {
    my ($input, $func) = @_;
    my @values;
    for my $elem ($input->@*) {
        push @values, $elem if $func->($elem);
    }
    return \@values;
}

sub discard {
    my ($input, $func) = @_;
    return keep($input, sub { return not $func->(shift) });
}

1;
