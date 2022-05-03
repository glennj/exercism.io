package Leap;


#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => is_leap_year';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ is_leap_year /;

sub is_leap_year {
    my ($year) = @_;
    return $year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0);
}

1;
