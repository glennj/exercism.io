package Triangle;
use strictures 2;
use List::Util qw/ product all /;

use Memoize;
memoize($_) for qw( row binom fact );

sub triangle {
    return [ map { row($_) } 0 .. shift ];
}

sub is_triangle {
    my $candidate = shift;
    # sadly the test suite expects "0" for false, not 
    # simply a perl false value.
    my $valid = all { $candidate->[$_] eq row($_) } 0 .. $candidate->$#*;
    return $valid ? 1 : 0;
}

sub row {
    my $n = shift;
    return join ' ', map { binom($n, $_) } 0 .. $n;
}

# binomial coefficient "n choose k"
sub binom {
    my ($n, $k) = @_;
    return 1  if $k == 0 or $n - $k == 0;
    return $n if $k == 1 or $n - $k == 1;
    return fact($n) / ( fact($k) * fact($n - $k) ) ;
}

sub fact {
    return product 1 .. shift;
};

1;
