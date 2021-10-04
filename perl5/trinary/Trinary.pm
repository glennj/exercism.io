package Trinary;

use strict;
use warnings;

our $BASE = 3;
our $DIGITS = join '', 0..$BASE-1;

sub new {
    my ($class, $num) = @_;
    $num =~ s/[^$DIGITS].*//x;
    # the object is a reference to the digit string itself.
    return bless \$num, $class;
}

sub to_decimal {
    my $self = shift;
    my $result = 0;
    $result = $result * $BASE + $_ for split //, $$self;
    return $result;
}

1;
