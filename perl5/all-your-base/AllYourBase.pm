package AllYourBase;

use 5.024;
use strictures 2;
use Exporter::Easy  (OK => [qw/ rebase /]);
use List::Util      qw/ reduce all /;
use Carp;

sub rebase {
    my ($iBase, $oBase, $digits) = (shift)->@{qw/inputBase outputBase digits/};

    croak 'input base must be >= 2'  unless $iBase >= 2;
    croak 'output base must be >= 2' unless $oBase >= 2;
    croak 'all digits must satisfy 0 <= d < input base'
        unless all {0 <= $_ and $_ < $iBase} @$digits;

    my $decimal = reduce { $a * $iBase + $b } 0, @$digits;

    my @to_digits;
    do {
        my $remainder = $decimal % $oBase;
        unshift @to_digits, $remainder;
        $decimal = ( $decimal - $remainder ) / $oBase;
    } while ($decimal > 0);

    return \@to_digits,
}
 
1;
