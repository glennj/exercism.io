#!perl
use 5.20.0;
use strict;
use warnings;
use feature qw/ postderef /;

package LinkedList;

## no critic (Subroutines::ProhibitBuiltinHomonyms)

sub new {
    my ($class, $value, $next) = @_;
    my $self = {
        value => $value,
        next => $next,
    };
    return bless $self, $class;
}

sub data { return shift->{value}; }

sub next { return shift->{next}; }

sub from_array {
    my ($class, $values) = @_;
    my $prev;
    for my $value (reverse $values->@*) {
        $prev = $class->new($value, $prev);
    }
    return $prev;
}

sub to_array {
    my ($self) = @_;
    my @values = ();
    do {
        push @values, $self->data();
    } while ($self = $self->next());
    return \@values;
}

sub reverse {
    my ($self) = @_;
    my $head;
    do {
        $head = (ref $self)->new( $self->data(), $head );
    } while ($self = $self->next());
    return $head;
}

1;
