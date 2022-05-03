package OCRNumbers;


#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => convert';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ convert /;

use Carp;
use List::Util  qw/ all /;

our %NUMBERS = (
    ' _ | ||_|' => 0,
    '     |  |' => 1,
    ' _  _||_ ' => 2,
    ' _  _| _|' => 3,
    '   |_|  |' => 4,
    ' _ |_  _|' => 5,
    ' _ |_ |_|' => 6,
    ' _   |  |' => 7,
    ' _ |_||_|' => 8,
    ' _ |_| _|' => 9,
);

sub convert {
    my ($lines) = @_;
    croak "Number of input lines is not a multiple of four" if @$lines % 4 != 0;

    # collect lines 0,1,2 and 4,5,6 and 8,9,10 and ... as "groups"
    my @groups = map {[ @$lines[4*$_ .. 4*$_+2] ]} 0 .. (@$lines / 4 - 1);

    for my $group (@groups) {
        croak "Number of input columns is not a multiple of three"
            if length($group->[0]) % 3 != 0;
        croak "Invalid input, line lengths differ" 
            unless all {length $_ == length $group->[0]} @$group;
    }

    return join ",", map {convert_group($_)} @groups;
}

sub convert_group {
    my ($group) = @_;
    my $digits = '';
    for (my $idx = 0; $idx < length $group->[0]; $idx += 3) {
        my $numstring = join "", map {substr $_, $idx, 3} @$group;
        $digits .= $NUMBERS{$numstring} // '?';
    }
    return $digits;
}

1;

