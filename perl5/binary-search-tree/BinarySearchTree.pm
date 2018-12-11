package BinarySearchTree;
use strict;
use warnings;

use Class::Tiny qw{ data left right };

sub BUILDARGS {
    my ($class, $value) = @_;
    return {data => $value};
}

sub insert {
    my ($self, $value) = @_;
    my $side = ($value <= $self->data) ? "left" : "right";
    if ($self->{$side}) { $self->{$side}->insert($value); } 
    else                { $self->{$side} = (ref $self)->new($value); }
    return $self;
}

## no critic (Subroutines::ProhibitBuiltinHomonyms)

sub each {
    my ($self, $callback) = @_;
    $self->left->each($callback) if $self->left;
    $callback->($self->data);
    $self->right->each($callback) if $self->right;
    return $self;
}


1;
