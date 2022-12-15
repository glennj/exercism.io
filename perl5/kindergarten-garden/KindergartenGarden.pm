## no critic (RegularExpressions::RequireExtendedFormatting)

package KindergartenGarden;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => plants';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ plants /;

use List::MoreUtils qw/ pairwise /;
use Carp;

our @STUDENTS = qw( Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry );
our $PLANTS = {G => 'grass', C => 'clover', R => 'radishes', V => 'violets'};

sub plants {
    my ($garden_code, $student) = @_;

    my @rows = split /\n/, $garden_code;
    my @pieces = pairwise { $a . $b } @{[ $rows[0] =~ /../g ]}, @{[ $rows[1] =~ /../g ]};
    my %garden = pairwise {
        $a => [map {$PLANTS->{$_}} split '', ($b//'')];
    } @STUDENTS, @pieces;

    return $garden{$student};
}

1;
