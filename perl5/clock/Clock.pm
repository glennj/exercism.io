#!perl
## no critic (Subroutines::ProhibitBuiltinHomonyms)

use 5.024;
use strictures 2;

package Clock;
use Class::Tiny     qw/ minutes /;

sub BUILDARGS {
    my ($class, $args) = @_;
    my ($hour, $minute) = $args->@{qw/hour minute/};
    return {minutes => _normalize(($hour // 0) * 60 + ($minute // 0))};
};

sub _normalize {
    my ($minutes) = @_;
    return $minutes % (24 * 60);
}

sub time {
    my ($self) = @_;
    my $hour = int($self->{minutes} / 60);
    my $minute = $self->{minutes} % 60;
    return sprintf("%02d:%02d", $hour, $minute);
}

sub add_minutes {
    my ($self, $amount) = @_;
    $self->{minutes} = _normalize($self->{minutes} + $amount);
    return $self;
}

sub subtract_minutes {
    my ($self, $amount) = @_;
    return $self->add_minutes( -$amount );
}

1;
