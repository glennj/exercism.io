package Series;

use 5.024;
use strictures 2;
use Exporter::Easiest 'OK => slices';
use Carp;

sub slices {
    my ($series, $size) = (shift)->@{'series', 'sliceLength'};
    my $len = length $series;

    croak "series cannot be empty" if $len == 0;
    croak "slice length cannot be zero" if $size == 0;
    croak "slice length cannot be negative" if $size < 0;
    croak "slice length cannot be greater than series length" if $size > $len;

    return [map {substr($series, $_, $size)} 0 .. ($len - $size)];
}

1;
