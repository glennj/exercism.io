package Hamming;

use strictures 2;
use Carp;
use List::MoreUtils qw/ zip /;
use List::Util      qw/ pairgrep /;

use Exporter 'import';
our @EXPORT_OK = qw/ hamming_distance /;

sub hamming_distance {
    my @a = split //, shift;
    my @b = split //, shift;
    croak "left and right strands must be of equal length" unless @a == @b;
    return scalar pairgrep {$a ne $b} zip @a, @b;
}

1;
