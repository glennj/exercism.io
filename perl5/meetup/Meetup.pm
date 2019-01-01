package Meetup;

use strict;
use warnings;
use Carp;
use DateTime;
use List::MoreUtils qw/ firstidx /;

sub new {
    my ($class, $month, $year) = @_;
    return bless { year => $year, month => $month }, $class;
}

sub day {
    my ($self, $weekday, $week) = @_;
    my $start = $week eq 'last'
        ? DateTime->last_day_of_month( %$self )->day - 6
        : {first => 1, second => 8, third => 15, fourth => 22, teenth => 13}->{$week};
    croak "unknown week type: $week" if not defined $start;

    my $dow = firstidx {lc $weekday eq $_} qw(monday tuesday wednesday thursday friday saturday sunday);
    croak "unknown weekday name: $weekday" if $dow == -1;

    my $datetime = DateTime->new( %$self, day => $start );
    return $datetime->add(days => ($dow - $datetime->day_of_week_0) % 7);
}

1;
