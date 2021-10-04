package BinarySearch;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => binary_search';
use Carp;
use feature      'current_sub';     # provides the "__SUB__" token
use POSIX        'ceil';
use Scalar::Util 'looks_like_number';
use List::Util   'all';

sub binary_search {
    my ($key, $list) = (shift)->@{qw/value array/};
    return binary_search_rec($key, $list, 0, $list->$#*);
}

sub binary_search_rec {
    my ($key, $list, $i, $j) = @_;
    croak "value not in array" if $i > $j;

    my $mid = ceil(($i + $j) / 2);
    my $val = $list->[$mid];
    return $mid if eql($key, $val);

    ($i, $j) = less_or_eq($key, $val) ? ($i, $mid-1) : ($mid+1, $j);
    return __SUB__->($key, $list, $i, $j);
}

sub eql {
    my ($a, $b) = @_;
    return numeric($a, $b) ? $a == $b : $a eq $b;
}

sub less_or_eq {
    my ($a, $b) = @_;
    return numeric($a, $b) ? $a <= $b : $a le $b;
}

sub numeric {
    my ($a, $b) = @_;
    return looks_like_number($a) && looks_like_number($b);
}

1;
