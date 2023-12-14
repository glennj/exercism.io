package LargestSeriesProduct;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => largest_product';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ largest_product /;

use List::Util qw/ max product /;
use Carp;

sub largest_product {
    my ($digits, $span) = @_;
    my @digits = split //, $digits;

    croak 'span must not be negative'               if $span < 0;
    croak 'span must be smaller than string length' unless $span <= @digits;
    croak 'digits input must only contain digits'   if $digits =~ /\D/;

    return max 
           map { product @digits[$_ .. ($_ + $span - 1)] }
           0 .. (@digits - $span);
}

1;
