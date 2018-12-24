package Trinary;

use strict;
use warnings;

our $BASE = 3;
our $DIGITS = join '', 0..$BASE-1;

sub new {
    my ($class, $num) = @_;
    # Like how perl numberifies a string, strip first non-digit
    # and all subsequent characters.
    $num =~ s/[^$DIGITS].*//;
    return bless \$num, $class;
}

sub to_decimal {
    my $self = shift;
    my $result = 0;
    $result = $result * $BASE + $_ for split //, $$self;
    return $result;
}

1;
