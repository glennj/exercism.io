package Gigasecond;

use strictures 2;
use Exporter::Easiest 'OK => add_gigasecond';

use DateTime;
use DateTime::Format::ISO8601;

our $GIGASECOND = DateTime::Duration->new( seconds => 1_000_000_000 );

sub add_gigasecond {
    my ($input) = @_;
    my $dt = DateTime::Format::ISO8601->parse_datetime($input);
    return ($dt + $GIGASECOND)->iso8601();
}

1;
