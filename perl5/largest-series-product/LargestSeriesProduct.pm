package LargestSeriesProduct;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => largest_product';
use List::Util qw/ max product /;
use Carp;

sub largest_product {
    my ($digits, $span) = (shift)->@{'digits', 'span'};
    my @digits = split //, $digits;

    croak 'span must be greater than zero'          unless $span >= 0;
    croak 'span must be smaller than string length' unless $span <= @digits;
    croak 'digits input must only contain digits'   if $digits =~ /\D/;

    return max 
           map { product @digits[$_ .. ($_ + $span - 1)] }
           0 .. (@digits - $span);
}

1;