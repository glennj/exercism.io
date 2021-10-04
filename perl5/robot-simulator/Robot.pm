#!perl

package Robot;

use strictures 2;
use List::Util  qw/ none first /;
use Carp;
use subs 'orient';

use Class::Tiny qw( orient coordinates );

our @BEARINGS = qw( north east south west );
our %ADVANCES = (
    north => [0, 1],  east => [1, 0],
    south => [0, -1], west => [-1, 0]
);

sub orient { 
    my ($self, $orientation) = @_;
    if (not defined $orientation) {
        return $self->{orient};
    }
    if (none {$orientation eq $_} @BEARINGS) {
        croak "ArgumentError: invalid orientation '$orientation'";
    }
    $self->{orient} = $orientation;
    return;
}

sub bearing { return shift->orient; }

sub turn_right { return shift->_turn_dir(+1); }
sub turn_left  { return shift->_turn_dir(-1); }

sub _turn_dir {
    my ($self, $direction) = @_;
    croak 'Error: robot has no orientation' unless $self->bearing;
    my $idx = first {$BEARINGS[$_] eq $self->bearing} 0..$#BEARINGS;
    $self->orient( $BEARINGS[($idx + $direction) % @BEARINGS] );
    return;
}

sub at {
    my ($self, $x, $y) = @_;
    $self->coordinates([$x, $y]);
    return;
}

sub advance {
    my ($self) = @_;
    my $step = $ADVANCES{ $self->bearing };
    my $loc = $self->coordinates;
    croak 'Error: robot has no coordinates' unless $loc;
    $self->at( $loc->[0] + $step->[0], $loc->[1] + $step->[1] );
    return;
}

1;
