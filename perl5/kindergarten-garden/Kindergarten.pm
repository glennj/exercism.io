package Kindergarten;

use strict;
use warnings;
no warnings 'redefine';
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

    # Inject a method for each student to return their plants.
    for my $student (map {lc} @$students) {
        my $func = join '::', __PACKAGE__, $student;
        no strict;
        *$func = sub { my $self = shift; return $self->{$student}; }
    }

    return bless \%garden, $class;
}

1;
