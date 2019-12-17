package Leap;

use strictures 2;
use Exporter 'import';
our @EXPORT_OK = qw(is_leap_year);

sub is_leap_year {
    my ($year) = @_;
    return $year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0);
}

1;
