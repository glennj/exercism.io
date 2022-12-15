package Gigasecond;

#use strictures 2;
use strict;
use warnings;
#use Exporter::Easiest 'OK => add_gigasecond';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ add_gigasecond /;

=begin

# as of Dec 2022, this fails the exercism perl5 test runner
# due to the unavailability of DateTime modules

use DateTime;
use DateTime::Format::ISO8601;

our $GIGASECOND = DateTime::Duration->new( seconds => 1_000_000_000 );

sub add_gigasecond {
    my ($input) = @_;
    my $dt = DateTime::Format::ISO8601->parse_datetime($input);
    return ($dt + $GIGASECOND)->iso8601();
}

=cut

# Time::Piece->strptime cannot perform "free-form" datetime parsing:
# the input format must be specified.
# But it will suffice for the test cases here.

use Time::Piece;

our $GIGASECOND = 1_000_000_000;

sub add_gigasecond {
    my ($input) = @_;
    my $dt = Time::Piece->strptime($input, "%FT%T");
    return ($dt + $GIGASECOND)->datetime;
}

1;
