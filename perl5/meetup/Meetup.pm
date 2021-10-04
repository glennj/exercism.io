package Meetup;

use strictures 2;
use Exporter::Easiest 'OK => meetup';
use Carp;
use DateTime;
use List::MoreUtils qw/ firstidx /;


sub meetup {
    my ($year, $month, $week, $weekday) = (shift)->@{qw/year month week dayofweek/};

    my $start = $week eq 'last'
        ? DateTime->last_day_of_month( year => $year, month => $month )->day - 6
        : {first => 1, second => 8, third => 15, fourth => 22, teenth => 13}->{$week};
    croak "unknown week type: $week" if not defined $start;

    my $dow = firstidx {lc $weekday eq $_} qw(monday tuesday wednesday thursday friday saturday sunday);
    croak "unknown weekday name: $weekday" if $dow == -1;

    my $datetime = DateTime->new( year => $year, month => $month, day => $start );

    return $datetime
        ->add(days => ($dow - $datetime->day_of_week_0) % 7)
        ->ymd();
}

1;
