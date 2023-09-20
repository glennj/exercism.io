#!perl

use v5.38;

package Robot;

use Moo;

has x => ( is => 'rwp' );
has y => ( is => 'rwp' );
has direction => ( is => 'rwp' );

sub enact($self, $instructions) {
    for my $instruction (split //, $instructions) {
        if    ($instruction eq "L") { $self->turnLeft }
        elsif ($instruction eq "R") { $self->turnRight }
        elsif ($instruction eq "A") { $self->advance }
    }
    return $self;
}

sub turnRight($self) {
    $self->_set_direction( {
        'north' => 'east',
        'east'  => 'south',
        'south' => 'west',
        'west'  => 'north',
    }->{$self->direction} );
}

sub turnLeft($self) {
    $self->_set_direction( {
        'north' => 'west',
        'west'  => 'south',
        'south' => 'east',
        'east'  => 'north',
    }->{$self->direction} );
}

sub advance($self) {
    if    ($self->direction eq 'north') { $self->_set_y( $self->y + 1) }
    elsif ($self->direction eq 'east')  { $self->_set_x( $self->x + 1) }
    elsif ($self->direction eq 'south') { $self->_set_y( $self->y - 1) }
    elsif ($self->direction eq 'west')  { $self->_set_x( $self->x - 1) }
}
