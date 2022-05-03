package BinarySearch;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => binary_search';
use Carp;
use feature      'current_sub';     # provides the "__SUB__" token
use POSIX        'ceil';
use Scalar::Util 'looks_like_number';
use List::Util   'all';

our ($EQ, $LE);

sub binary_search {
    my ($key, $list) = (shift)->@{qw/value array/};
    ($EQ, $LE) = initialize($key, @$list);
    return binary_search_rec($key, $list, 0, $list->$#*);
}

sub initialize {
    if (all {looks_like_number $_} @_) {
        return (
            sub { return $_[0] == $_[1] },
            sub { return $_[0] <= $_[1] },
        );
    }
    else {
        return (
            sub { return $_[0] eq $_[1] },
            sub { return $_[0] le $_[1] },
        );
    }
}

sub binary_search_rec {
    my ($key, $list, $i, $j) = @_;
    croak "value not in array" if $i > $j;

    my $mid = ceil(($i + $j) / 2);
    my $val = $list->[$mid];
    return $mid if $EQ->($key, $val);

    ($i, $j) = $LE->($key, $val) ? ($i, $mid-1) : ($mid+1, $j);
    return __SUB__->($key, $list, $i, $j);
}

1;
