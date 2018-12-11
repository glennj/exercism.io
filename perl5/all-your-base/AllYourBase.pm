package AllYourBase;
use strictures 2;
use List::Util qw/ reduce any /;
use Carp;

sub convert_base {
    my ($from_digits, $from_base, $to_base) = @_;

    croak 'base must be greater than 1'          if $from_base <= 1 or $to_base <= 1;
    croak 'negative digit not allowed'           if any {$_ < 0} @$from_digits;
    croak 'digit equal of greater than the base' if any {$_ >= $from_base} @$from_digits;

    my @r_digits = reverse @$from_digits;
    my $decimal = reduce
        { $a + $r_digits[$b] * ($from_base ** $b) }
        0,
        0 .. $#r_digits;

    my @to_digits;
    do {
        my $remainder = $decimal % $to_base;
        unshift @to_digits, $remainder;
        $decimal = ( $decimal - $remainder ) / $to_base;
    } while ($decimal > 0);

    return \@to_digits,
}

1;
