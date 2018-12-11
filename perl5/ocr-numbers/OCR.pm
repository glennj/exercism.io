package OCR;

## no critic (RegularExpressions::RequireExtendedFormatting)

use strict;
use warnings;
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

sub new {
    my ($class, $input) = @_;
    my @lines = split /\n/, $input;
    croak "Invalid input, not multiple of 4 lines" if @lines % 4 != 0;

    # collect lines 0,1,2 and 4,5,6 and 8,9,10 and ... as "groups"
    my @groups = map {[ @lines[4*$_ .. 4*$_+2] ]} 0 .. (@lines / 4 - 1);

    for my $group (@groups) {
        croak "Invalid input, line length not multiple of 3"
            if length($group->[0]) % 3 != 0;
        croak "Invalid input, line lengths differ" 
            unless all {length $_ == length $group->[0]} @$group;
    }
    return bless \@groups, $class;
}

sub convert {
    my ($self) = @_;
    return join ",", map {$self->convert_group($_)} @$self;
}

sub convert_group {
    my ($self, $group) = @_;
    my $digits = '';
    for (my $idx = 0; $idx < length $group->[0]; $idx += 3) {
        my $numstring = join "", map {substr $_, $idx, 3} @$group;
        $digits .= $NUMBERS{$numstring} // '?';
    }
    return $digits;
}

1;
