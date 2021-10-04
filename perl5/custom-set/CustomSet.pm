#!perl
use strict;
use warnings;
use feature qw/ postderef /;

use JSON::PP;

package CustomSet;

sub new {
    my ($class, @args) = @_;
    my $data = {};
    $data->{$_} = 1 for @args;
    return bless $data, $class;
}

sub to_string {
    my ($self) = @_;
    return JSON::PP::encode_json([ sort $self->to_list() ]);
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

sub is_equal {
    my ($self, $other) = @_;
    return $self->size() == $other->size()
        && $self->is_subset($other);
}

sub is_member {
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
    return (ref $self)->new( $self->to_list(), $other->to_list() );
}

sub intersect {
    my ($self, $other) = @_;
    my @common = grep {$other->is_member($_)} $self->to_list();
    return (ref $self)->new(@common);
}

sub difference {
    my ($self, $other) = @_;
    my $clone = (ref $self)->new($self->to_list());
    $clone->remove($_) for $other->to_list();
    return $clone;
}

sub is_disjoint {
    my ($self, $other) = @_;
    return $self->intersect($other)->is_empty();
}

sub is_subset {
    my ($self, $other) = @_;
    return $self->size() >= $other->size()
        && $other->size() == $other->intersect($self)->size();
}

1;
