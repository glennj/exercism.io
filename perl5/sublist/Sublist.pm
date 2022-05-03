package Sublist;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => compare_lists';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ compare_lists /;

## no critic (ValuesAndExpressions::ProhibitConstantPragma)
use constant False => 0;
use constant True  => 1;

=begin
# This method will be unrealiable, assuming the join character
# does not appear in the data.

sub compare_lists {
    my ($one, $two) = (shift)->@{'listOne', 'listTwo'}; 
    # stringify the lists: join with ASCII "unit separator" char
    my $x = join "\x1f", @$one;
    my $y = join "\x1f", @$two;

    return "equal"     if $x eq $y;
    return "sublist"   if index($y, $x) > -1;
    return "superlist" if index($x, $y) > -1;
    return "unequal";
}
=cut

sub contains;

sub compare_lists {
    my ($one, $two) = (shift)->@{'listOne', 'listTwo'}; 

    if (contains $one, $two) {
        return "equal" if $one->$#* == $two->$#*;
        return "superlist";
    } elsif (contains $two, $one) {
        return "sublist";
    } else {
        return "unequal";
    }
}

sub contains {
    my ($parent, $child) = @_;
    my ($p_len, $c_len) = (scalar(@$parent), scalar(@$child));

    # every list contains the empty list
    return True if $c_len == 0;

    my $i = 0;
    my $found_sublist;
    while ($i < ($p_len - $c_len + 1)) {
        if ($parent->[$i] == $child->[0]) {
            $found_sublist = True;
            for my $j (1 .. ($c_len - 1)) {
                if ($parent->[$i + $j] != $child->[$j]) {
                    $found_sublist = False;
                    last;
                }
            }
            return 1 if $found_sublist;
        }
        $i++;
    }
    return False;
}

1;
