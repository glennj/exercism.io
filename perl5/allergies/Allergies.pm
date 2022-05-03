package Allergies;
use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => allergic_to list_allergies';
use Exporter     qw/ import /;
our @EXPORT_OK = qw/ allergic_to list_allergies /;

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
    my ($input) = @_;
    # delegate to a subroutine with a simpler signature
    return is_allergic_to($input->{score}, $input->{item});
}

sub is_allergic_to {
    my ($score, $allergen) = @_;
    return !!($score & $ALLERGIES{$allergen});
}

sub list_allergies {
    my ($score) = @_;
    return [grep {is_allergic_to($score, $_)} @ALLERGENS];
}

1;
