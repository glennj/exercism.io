package Sublist;

use v5.38;

use Exporter qw/ import /;
our @EXPORT_OK = qw/ compare_lists /;

use constant False => 0;
use constant True  => 1;

sub contains;

sub compare_lists($one, $two) {
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
