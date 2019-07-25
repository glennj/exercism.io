package Triangle;
use strictures 2;
use Carp;
use List::Util  qw/ any /;

sub kind {
    croak 'TriangleError' if any {$_ <= 0} @_;

    my ($i, $j, $k) = sort {$a <=> $b} @_;

    croak 'TriangleError' if $i + $j <= $k;

    return 'equilateral' if $i == $j and $i == $k;
    return 'isosceles'   if $i == $j or $i == $k or $j == $k;
    return 'scalene';
}

1;
