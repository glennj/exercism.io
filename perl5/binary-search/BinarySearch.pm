package BinarySearch;

use 5.024;
#use strictures 2;
use strict;
use warnings;
#use Exporter::Easiest 'OK => binary_search';
use Exporter        qw/ import /;
our @EXPORT_OK =    qw/ binary_search /;

use Carp;
use feature         qw/ current_sub /;     # provides the "__SUB__" token
use POSIX           qw/ ceil /;
use Scalar::Util    qw/ looks_like_number /;
use List::Util      qw/ all /;

our ($EQ, $LE);

sub binary_search {
    my ($list, $key) = @_;
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
