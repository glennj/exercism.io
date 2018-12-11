package Hexadecimal;

use strict;
use warnings;
use List::Util 'reduce';

our $DIGITS = '0123456879abcdef';

sub new {
    my ($class, $hex) = @_;
    return bless [split '', lc $hex], $class;
}

sub to_decimal {
    return reduce {
        my $value = index $DIGITS, $b;
        return 0 if $value == -1;
        $a * length($DIGITS) + $value;
    } 0, shift->@*;
}

1;
