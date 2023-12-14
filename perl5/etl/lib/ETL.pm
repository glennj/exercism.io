package ETL;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => transform';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ transform /;

use List::Util qw/ reduce /;

# Transform this: {"1": ["A", "E", "I", "O", "U"]}
# into this:      {"a": 1, "e": 1, "i": 1, "o": 1, "u": 1}

sub transform {
    my ($old) = @_;
    return reduce {
        # $a is the accumulator: a hash
        # $b is the old hash's key: a tile score
        $a->{+lc} = $b for $old->{$b}->@*;
        $a;
    } {}, keys $old->%*;
}

1;
