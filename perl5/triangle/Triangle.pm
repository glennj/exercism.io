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

__END__

@choroba's solution:

    my %lengths;
    undef @lengths{$x, $y, $z};   # We care only about the keys.

    # The number of different lengths tells us the type of the triangle.
    return { 1 => 'equilateral',
             2 => 'isosceles',
             3 => 'scalene',
            }->{ keys %lengths }

