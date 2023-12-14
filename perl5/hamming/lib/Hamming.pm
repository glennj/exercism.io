package Hamming;

# as of Dec 2022, this fails the exercism perl5 test runner
# due to the unavailability of List::MoreUtils.

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => hamming_distance';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ hamming_distance /;

use Carp;
use List::Util      qw/ pairgrep /;
use List::MoreUtils qw/ zip /;

sub hamming_distance {
    my @a = split //, shift;
    my @b = split //, shift;
    croak "left and right strands must be of equal length" unless @a == @b;
    return scalar pairgrep {$a ne $b} zip @a, @b;
}

1;
