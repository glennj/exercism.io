#!perl

use 5.024;
use strictures 2;

package LinkedList;


use Moo;

has data => (
    is => 'ro',
);
has next => (
    is => 'rwp',
);

sub BUILDARGS {
    my ($class, $value, $next) = @_;
    return { data => $value, next => $next };
}

sub from_array {
    my ($class, $values) = @_;
    my $head;
    for my $value (reverse $values->@*) {
        $head = $class->new($value, $head);
    }
    return $head;
}

sub to_array {
    my ($self) = @_;
    my @values = ();
    do {
        push @values, $self->data;
    } while ($self = $self->next);
    return \@values;
}

## no critic (Subroutines::ProhibitBuiltinHomonyms)
sub reverse {
    my ($self) = @_;
    my $head;
    do {
        $head = (ref $self)->new( $self->data, $head );
    } while ($self = $self->next);
    return $head;
}

1;
