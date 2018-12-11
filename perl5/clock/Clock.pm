#!perl
## no critic (Subroutines::ProhibitBuiltinHomonyms)

package Clock;

use strict;
use warnings;

our $MIN_PER_DAY = 24 * 60;

sub new {
  my ($class, $attributes) = @_;
  $attributes->{hour} //= 0;
  $attributes->{minute} //= 0;
  $attributes->{m} = $attributes->{hour} * 60 + $attributes->{minute};
  bless $attributes, $class;
  $attributes->_normalize();
  return $attributes;
}

sub time {
  my ($self) = @_;
  return sprintf("%02d:%02d", $self->{hour}, $self->{minute});
}

sub _normalize {
  my ($self) = @_;
  $self->{m} %= $MIN_PER_DAY;
  $self->{hour} = int($self->{m} / 60);
  $self->{minute} = $self->{m} % 60;
  return;
}

sub add_minutes {
  my ($self, $amount) = @_;
  $self->{m} += $amount;
  $self->_normalize();
  return $self;
}

sub subtract_minutes {
  my ($self, $amount) = @_;
  return $self->add_minutes( -$amount );
}

1;
