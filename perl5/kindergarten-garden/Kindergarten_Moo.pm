package Kindergarten_Moo;

use strict;
use warnings;
use List::MoreUtils qw/ pairwise /;
use Carp;
use Moo;

## no critic (RegularExpressions::RequireExtendedFormatting)

has plants => (
    is => 'ro',
    default => sub { {G => 'grass', C => 'clover', R => 'radishes', V => 'violets'} },
);

has students => (
    is => 'ro',
    default => sub { [qw( Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry )] },
);

has garden_code => (
    is => 'ro',
    required => 1,
);

has garden => (
    is => 'lazy',
);

sub _build_garden {         ## no critic (ProhibitUnusedPrivateSubroutines)
    my ($self) = @_;
    my @rows = split /\n/, $self->garden_code;
    my @pieces = pairwise { $a . $b } @{[ $rows[0] =~ /../g ]}, @{[ $rows[1] =~ /../g ]};
    my %garden = pairwise {
        lc $a => [map {$self->plants->{$_}} split '', ($b//'')];
    } @{$self->students}, @pieces;
    return \%garden;
}

around BUILDARGS => sub {
    my ($orig, $class, $gardenstr, $students) = @_;
    my %args = (garden_code => $gardenstr);
    $args{students} = [sort @$students] if $students;
    return $class->$orig(%args);
};

# Handle student names as object methods.
# there's gotta be a better way...
sub AUTOLOAD {                      ## no critic (ProhibitAutoloading)
    my ($self) = @_;
    (my $student = our $AUTOLOAD) =~ s/.*:://;
    return $self->garden->{lc $student} // "";
}

1;
