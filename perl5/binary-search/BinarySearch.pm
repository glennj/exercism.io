package BinarySearch;

use 5.024;
use strictures 2;
use Carp;
use feature      'current_sub';
use POSIX        'ceil';
use Scalar::Util 'looks_like_number';
use List::Util   'all';

use Exporter     'import';
our @EXPORT_OK = qw/ binary_search /;

sub binary_search {
    my ($args) = @_;
    my ($key, $list) = $args->@{qw/value array/};
    croak 'Unsorted list' unless sorted($list);
    return binary_search_rec($key, $list, 0, $list->$#*);
}

sub binary_search_rec {
    my ($key, $list, $i, $j) = @_;
    croak "value not in array" if $i > $j;

    my $mid = ceil(($i + $j) / 2);
    my $val = $list->[$mid];
    return $mid if equal($key, $val);

    ($i, $j) = less_or_eq($key, $val) ? ($i, $mid-1) : ($mid+1, $j);
    return __SUB__->($key, $list, $i, $j);
}

sub sorted {
    my $list = shift;
    return all { less_or_eq( $list->[$_-1], $list->[$_] ) } 1 .. $list->$#*;
}

sub equal {
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
