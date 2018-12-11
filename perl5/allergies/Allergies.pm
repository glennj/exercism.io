package Allergies;
use strict;
use warnings;
use List::Util  qw/ pairs /;

our %ALLERGIES = (
    eggs         => 0b00000001,
    peanuts      => 0b00000010,
    shellfish    => 0b00000100,
    strawberries => 0b00001000,
    tomatoes     => 0b00010000,
    chocolate    => 0b00100000,
    pollen       => 0b01000000,
    cats         => 0b10000000,
);
our @ALLERGENS = map {$_->key} sort {$a->value <=> $b->value} pairs %ALLERGIES;

use Class::Tiny qw/ score /;
sub BUILDARGS { return {score => pop} }

sub allergic_to {
    my ($self, $allergen) = @_;
    return $self->score & $ALLERGIES{$allergen};
}

sub list {
    my ($self) = @_;
    return [grep {$self->allergic_to($_)} @ALLERGENS];
}

1;
