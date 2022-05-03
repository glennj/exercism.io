package TwelveDays;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => recite';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ recite /;

my @gifts = (
    [],
    ['first',    'a Partridge in a Pear Tree'],
    ['second',   'two Turtle Doves'],
    ['third',    'three French Hens'],
    ['fourth',   'four Calling Birds'],
    ['fifth',    'five Gold Rings'],
    ['sixth',    'six Geese-a-Laying'],
    ['seventh',  'seven Swans-a-Swimming'],
    ['eighth',   'eight Maids-a-Milking'],
    ['ninth',    'nine Ladies Dancing'],
    ['tenth',    'ten Lords-a-Leaping'],
    ['eleventh', 'eleven Pipers Piping'],
    ['twelfth',  'twelve Drummers Drumming'],
);

sub verse {
    my ($n) = @_;
    return 
        "On the $gifts[$n][0] day of Christmas my true love gave to me: "
        . join(', ', map {$gifts[$_][1]} reverse 2 .. $n)
        . ($n > 1 ? ', and ' : '') 
        . $gifts[1][1]
        . '.';
}

sub recite {
    my ($start, $end) = (shift)->@{'start', 'end'};
    return join "\n", map {verse $_} $start .. $end;
}

1;
