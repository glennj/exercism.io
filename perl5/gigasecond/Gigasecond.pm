package Gigasecond;

use strict;
use warnings;
use DateTime;

our $GIGASECOND = DateTime::Duration->new( seconds => 1_000_000_000 );

use subs 'date';
use Class::Tiny qw( date );
sub BUILDARGS {
    my ($class, $year, $month, $day)  = @_;
    return {date => DateTime->new(year=>$year, month=>$month, day=>$day)};
}
sub date { return ( shift->{date} + $GIGASECOND )->truncate(to => 'day') }

1;
