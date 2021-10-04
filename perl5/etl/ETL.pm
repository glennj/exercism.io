package ETL;

use strictures 2;
use Exporter::Easiest 'OK => transform';
use List::Util qw/ reduce /;

# Transform this: {"1": ["A", "E", "I", "O", "U"]}
# into this:      {"a": 1, "e": 1, "i": 1, "o": 1, "u": 1}

sub transform {
    my ($old) = @_;
    return reduce {
        # $a is the accumulator: a hash
        # $b is the old hash's key
        $a->{+lc} = $b for @{ $old->{$b} };
        $a;
    } {}, keys %$old;
}

1;
