#!perl

use strict;
use warnings;

package CustomSet;

sub new {
    my ($class, %args) = @_;
    my $data = {};
    $data->{$_} = 1 for $args{elements}->@*;
    return bless $data, $class;
}

sub to_list { 
    my ($self) = @_;
    return keys $self->%*;
}

sub size {
    my ($self) = @_;
    return scalar $self->to_list();
}

sub is_empty {
    my ($self) = @_;
    return $self->size() == 0;
}

sub is_equal_to {
    my ($self, $other) = @_;
    return $self->size() == $other->size()
        && $self->is_subset_of($other);
}

sub contains {
    my ($self, $item) = @_;
    return !!$self->{$item};
}

sub add {
    my ($self, $item) = @_;
    $self->{$item} = 1;
    return $self;
}

sub remove {
    my ($self, $item) = @_;
    my $value = delete $self->{$item};
    return $self;
}

sub empty {
    my ($self) = @_;
    $self->remove($_) for $self->to_list();
    return $self;
}

sub union {
    my ($self, $other) = @_;
    return (ref $self)->new(elements => [$self->to_list(), $other->to_list()]);
}

sub intersection {
    my ($self, $other) = @_;
    my @common = grep {$other->contains($_)} $self->to_list();
    return (ref $self)->new(elements => \@common);
}

sub difference {
    my ($self, $other) = @_;
    my $clone = (ref $self)->new(elements => [$self->to_list()]);
    $clone->remove($_) for $other->to_list();
    return $clone;
}

sub is_disjoint_of {
    my ($self, $other) = @_;
    return $self->intersection($other)->is_empty();
}

sub is_subset_of {
    my ($self, $other) = @_;
    return $self->size() <= $other->size()
        && $self->size() == $other->intersection($self)->size();
}

1;
