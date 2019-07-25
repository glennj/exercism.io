package ETL;

use strict;
use warnings;
use List::Util qw/ reduce /;

sub transform {
    my ($old) = @_;
    return reduce {
        $a->{+lc} = $b for @{ $old->{$b} };
        $a;
    } {}, keys %$old;
}

1;
