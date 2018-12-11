package Hamming;
use strictures 2;
use Carp;

sub compute {
    my @a = split //, shift;
    my @b = split //, shift;
    croak "DNA strands must be of equal length" unless @a == @b;
    return scalar grep {$a[$_] ne $b[$_]} 0 .. $#a;
}

1;
