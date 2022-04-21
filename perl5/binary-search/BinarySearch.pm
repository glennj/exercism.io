package BinarySearch;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => binary_search';
use Carp;
use feature      'current_sub';     # provides the "__SUB__" token
use POSIX        'ceil';

sub binary_search {
    my ($key, $list) = (shift)->@{qw/value array/};
    return binary_search_rec($key, $list, 0, $list->$#*);
}

sub binary_search_rec {
    my ($key, $list, $i, $j) = @_;
    croak "value not in array" if $i > $j;

    my $mid = ceil(($i + $j) / 2);
    my $val = $list->[$mid];
    return $mid if $key == $val;

    ($i, $j) = $key < $val ? ($i, $mid-1) : ($mid+1, $j);
    return __SUB__->($key, $list, $i, $j);
}

1;
