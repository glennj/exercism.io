package Kindergarten;

use strict;
use warnings;
use List::MoreUtils qw/ pairwise /;
use Carp;
## no critic (RegularExpressions::RequireExtendedFormatting)

our $DEF_STUDENTS = [qw( Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry )];
our $PLANTS = {G => 'grass', C => 'clover', R => 'radishes', V => 'violets'};

sub new {
    my ($class, $garden_code, $students) = @_;
    $students = [sort @{ $students // $DEF_STUDENTS }];

    my @rows = split /\n/, $garden_code;
    my @pieces = pairwise { $a . $b } @{[ $rows[0] =~ /../g ]}, @{[ $rows[1] =~ /../g ]};
    my %garden = pairwise {
        lc $a => [map {$PLANTS->{$_}} split '', ($b//'')];
    } @$students, @pieces;
    return bless \%garden, $class;
}

# Handle student names as object methods.
# there's gotta be a better way...
sub AUTOLOAD {                      ## no critic (ProhibitAutoloading)
    my ($self) = @_;
    (my $student = our $AUTOLOAD) =~ s/.*:://;
    return $self->{lc $student} // "";
}

1;
