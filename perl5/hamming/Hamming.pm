package Hamming;

use strictures 2;
use Carp;

use Exporter 'import';
our @EXPORT_OK = qw/ hamming_distance /;

sub hamming_distance {
    my @a = split //, shift;
    my @b = split //, shift;
    croak "left and right strands must be of equal length" unless @a == @b;
    return scalar grep {$a[$_] ne $b[$_]} 0 .. $#a;
}

1;
