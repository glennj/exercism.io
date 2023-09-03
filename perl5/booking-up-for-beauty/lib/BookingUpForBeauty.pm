package BookingUpForBeauty;

use v5.38;

use Time::Piece;
use Readonly;

use Exporter     qw(import);
our @EXPORT_OK = qw(appointment_has_passed  is_afternoon_appointment  describe_appointment);

my $STRPTIME_FORMAT = '%Y-%m-%d' . 'T' . '%H:%M:%S';
Readonly::Scalar $STRPTIME_FORMAT => $STRPTIME_FORMAT;

# Private subroutines conventionally start with an underscore.
# It isn't necessary, but provided for convenience.
sub _parse_datetime ($date_string) {
    return Time::Piece->strptime($date_string, $STRPTIME_FORMAT);
}

sub appointment_has_passed ($date_string) {
    my $time = _parse_datetime($date_string);
    my $now = localtime;
    return $time < $now;
}

sub is_afternoon_appointment ($date_string) {
    my $time = _parse_datetime($date_string);
    my $hour = $time->hour;
    return 12 <= $hour && $hour < 18 ;
}

sub describe_appointment ($date_string) {
    my $time = _parse_datetime($date_string);
    return $time->strftime('You have an appointment on %m/%d/%Y %-I:%M %p');
}
