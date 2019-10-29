package PascalsTriangle;
use strictures 2;
use Exporter 'import';
our @EXPORT_OK = qw/ pascal_rows /;

use List::Util qw/ product /;
use Memoize;
memoize($_) for qw( row binom fact );

sub pascal_rows {
    my $n = shift;
    return join "\n", map { row($_) } 0 .. $n-1;
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
