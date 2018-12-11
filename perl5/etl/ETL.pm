package ETL;

use strict;
use warnings;
use List::Util qw/ reduce pairmap /;

sub transform {
    return reduce {
        $a->{+lc} = $b for @{ $old->{$b} };
        $a;
    } {}, keys %$old;
}

1;

__END__

better:

usr List::Util qw/ pairmap /;
sub transform {
    return { pairmap { map {lc $_ => $a} @$b } shift->%* }
}
