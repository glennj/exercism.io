#!perl

package Simulator;

use strict;
use warnings;
use Carp;

use Class::Tiny;

sub place {
    my ($self, $robot, $attributes) = @_;
    if (exists $attributes->{direction}) {
        $robot->orient( $attributes->{direction} );
    }
    if (exists $attributes->{x} and exists $attributes->{y}) {
        $robot->at( $attributes->{x}, $attributes->{y} );
    }
    return;
}

sub evaluate {
    my ($self, $robot, $script) = @_;
    for my $instruction ( $self->instructions($script)->@* ) {
        $robot->$instruction;
    };
    return;
}

sub instructions {
    my ($self, $script) = @_;
    my %cookbook = (L => 'turn_left', R => 'turn_right', A => 'advance');
    my @result;
    for my $key (split //, $script) {
        my $instr = $cookbook{$key};
        croak "Error: invalid instruction '$key'" unless $instr;
        push @result, $instr;
    }
    return \@result;
}

1;
