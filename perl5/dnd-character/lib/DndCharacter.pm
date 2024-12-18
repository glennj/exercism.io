package DndCharacter;

use v5.40;
use Moo;
use strict;
use warnings;
use List::Util qw<sum min>;
use POSIX      qw<floor>;

sub ability ($self) {
    my @dice = map { 1 + int rand 6 } 1 .. 4;
    return sum(@dice) - min(@dice);
}

sub modifier ( $self, $value ) {
    return floor( ( $value - 10 ) / 2 );
}

has [qw< strength dexterity constitution intelligence wisdom charisma >] => (
    is      => 'lazy',
    builder => sub ($self) { $self->ability },
);

has hitpoints => (
    is      => 'lazy',
    builder => sub ($self) { 10 + $self->modifier( $self->constitution ) },
);

1;
