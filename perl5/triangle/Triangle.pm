package Triangle;

use strictures 2;
use Exporter::Easy (
    OK => [qw/ is_equilateral is_isosceles is_scalene /]
);
use List::Util  qw/ any /;

## no critic (ValuesAndExpressions::ProhibitConstantPragma)
use constant Equilateral => 3;
use constant Isosceles   => 2;
use constant Scalene     => 0;
use constant TriangleErr => -1;

sub type {
    my @sides = @_;
    return TriangleErr if any {$_ <= 0} @sides;

    my ($i, $j, $k) = sort {$a <=> $b} @sides;
    return TriangleErr if $i + $j <= $k;

    return Equilateral if $i == $j and $i == $k;
    return Isosceles   if $i == $j or $i == $k or $j == $k;
    return Scalene;
}

sub classify {
    my ($sides, $types) = @_;
    my $type = type @$sides;
    return grep {$type == $_} @$types;
}

## no critic (Subroutines::RequireFinalReturn)
sub is_equilateral { classify shift, [Equilateral] }
sub is_isosceles   { classify shift, [Isosceles, Equilateral] }
sub is_scalene     { classify shift, [Scalene] }

1;
