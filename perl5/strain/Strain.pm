package Strain;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => keep discard';

=begin

These function cheat the spirit of the exercise by using
the builtin `grep` function.

sub keepCheat {
    my ($input, $func) = @_;
    return [grep {$func->($_)} $input->@*];
}

sub discardCheat {
    my ($input, $func) = @_;
    return [grep {!$func->($_)} $input->@*];
}

=cut

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
