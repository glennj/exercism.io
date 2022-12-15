use 5.024;

#use strictures 2;
use strict;
use warnings;

use feature 'current_sub';

package BinarySearchTree::Node;
use Class::Tiny qw{ data left right };

sub insert {
    my ($self, $value) = @_;
    my $side = ($value <= $self->data) ? "left" : "right";
    if ($self->{$side}) { $self->{$side}->insert($value); } 
    else                { $self->{$side} = (ref $self)->new(data => $value); }
    return $self;
}

sub walk {
    my ($self, $callback) = @_;
    $self->left->walk($callback) if $self->left;
    $callback->($self);
    $self->right->walk($callback) if $self->right;
    return $self;
}

package BinarySearchTree;
use Class::Tiny qw{ root };

sub add {
    my ($self, $item) = @_;
    $self->root->insert($item);
}

sub sort {
    my $self = shift;
    my @sorted;
    $self->root->walk(sub {my $node = shift; push @sorted, $node->data});
    return \@sorted;
}

1;

