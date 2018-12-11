package Strain;

use strict;
use warnings;
use feature 'postderef';

sub keepCheat {
    my ($input, $func) = @_;
    return [grep {$func->($_)} $input->@*];
}

sub discardCheat {
    my ($input, $func) = @_;
    return [grep {!$func->($_)} $input->@*];
}

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
