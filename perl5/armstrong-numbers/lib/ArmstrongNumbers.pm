package ArmstrongNumbers;

## no critic (Subroutines::ProhibitSubroutinePrototypes, ValuesAndExpressions::ProhibitVersionStrings)

use v5.40;
use POSIX qw(floor log10);

use Exporter qw<import>;
our @EXPORT_OK = qw<is_armstrong_number>;

sub num_length ($n) {
    return 1 + floor( log10($n) );
}

sub divmod ( $n, $d ) {
    return ( int( $n / $d ), $n % $d );
}

sub armstrong_sum ($n) {
    my $sum = 0;
    my $len = num_length $n;
    my $digit;

    while ( $n > 0 ) {
        ( $n, $digit ) = divmod( $n, 10 );
        $sum += $digit**$len;
    }

    return $sum;
}

sub is_armstrong_number ($number) {
    return $number == armstrong_sum($number);
}

1;
