package SumOfMultiples;

use strict;
use warnings;
use List::Util  qw/ any sum0 /;

sub new {
    my ($class, @factors) = @_;
    return bless \@factors, $class;
}

sub to {
    my ($self, $limit) = @_;
    return sum0 grep {my $n=$_; any {$n % $_ == 0} @$self} 1 .. $limit-1;
}

1;
