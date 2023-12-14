package Series;

use v5.38;

use Exporter qw/ import /;
our @EXPORT_OK = qw/ slices /;

use Carp;

sub slices($series, $size) {
    my $len = length $series;

    croak "series cannot be empty" if $len == 0;
    croak "slice length cannot be zero" if $size == 0;
    croak "slice length cannot be negative" if $size < 0;
    croak "slice length cannot be greater than series length" if $size > $len;

    return [map {substr($series, $_, $size)} 0 .. ($len - $size)];
}

1;
