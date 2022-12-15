package GradeSchool;
use strict;
use warnings;

use feature qw/ postderef /;

use Moo;

has directory => (
    is => 'rwp',
    default => sub { {} },
);

sub roster {
    my ($self, $grade) = @_;
    return [
        map  { $_->[0] }
        sort { ($a->[1] <=> $b->[1]) || ($a->[0] cmp $b->[0]) }
        grep { (defined $grade) ? ($_->[1] == $grade) : 1 }
        map  { [$_, $self->directory->{$_}] }
        keys $self->directory->%*
    ];
}

sub add {
    my ($self, $name, $grade) = @_;
    return 0 if exists $self->directory->{$name};

    $self->directory->{$name} = $grade;
    return 1;
}

1;
