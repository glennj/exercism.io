#!perl
use strict;
use warnings;

package Sublist;

sub check_lists {
    # stringify the lists: join with ASCII "unit separator" char
    my $x = join "\x1f", @{+shift};
    my $y = join "\x1f", @{+shift};

    return "equal"     if $x eq $y;
    return "sublist"   if index($y, $x) > -1;
    return "superlist" if index($x, $y) > -1;
    return "unequal";
}

1;
