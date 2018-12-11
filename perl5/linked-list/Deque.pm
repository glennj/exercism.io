#!perl
## no critic (Subroutines::ProhibitBuiltinHomonyms, Modules::ProhibitMultiplePackages)

use strict;
use warnings;

package Node;
use Class::Tiny qw/ value prev next /;

package Deque;
use Class::Tiny qw/ head tail /;

sub unshift {
    my ($self, $value) = @_;
    my $node = Node->new( value => $value, next => $self->head );
    if ($self->head) {
        $node->next( $self->head );
        $self->head->prev( $node );
    }
    $self->head( $node );
    $self->tail( $node ) if not defined $self->tail;
    return $self;
}

sub shift {
    my ($self) = @_;
    my $value;
    if ($self->head) {
        $value = $self->head->value;
        $self->head( $self->head->next );
        $self->tail( undef ) if not defined $self->head;
    }
    return $value;
}

sub push {
    my ($self, $value) = @_;
    my $node = Node->new( value => $value, prev => $self->tail );
    if ($self->tail) {
        $node->prev( $self->tail );
        $self->tail->next( $node );
    }
    $self->tail( $node );
    $self->head( $node ) if not defined $self->head;
    return $self;
}

sub pop {
    my ($self) = @_;
    my $value;
    if ($self->tail) {
        $value = $self->tail->value;
        $self->tail( $self->tail->prev );
        $self->head( undef ) if not defined $self->tail;
    }
    return $value;
}

1;
