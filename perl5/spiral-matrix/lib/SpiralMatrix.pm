package SpiralMatrix;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<spiral_matrix>;

sub spiral_matrix ($size) {
    my @m = map { [ (undef) x $size ] } 1 .. $size;
    my ( $x, $y, $dx, $dy ) = ( 0, 0, 0, 1 );

    foreach my $i ( 1 .. ( $size * $size ) ) {
        $m[$x][$y] = $i;

        ( $dx, $dy ) = ( $dy, -$dx ) if ( $x + $dx == $size
                                       || $y + $dy == $size
                                       || $y + $dy < 0
                                       || defined $m[ $x + $dx ][ $y + $dy ] );
        $x += $dx;
        $y += $dy;
    }

    return \@m;
}

1;
