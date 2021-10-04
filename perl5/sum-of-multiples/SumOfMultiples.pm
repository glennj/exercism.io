package SumOfMultiples;

use strictures 2;
use Exporter::Easiest 'OK => sum_of_multiples';
use List::Util  qw/ any sum0 /;

sub sum_of_multiples {
    my ($factors, $limit) = (shift)->@{'factors', 'limit'};
    return sum0 
           grep {
               my $n = $_;
               any {$n % $_ == 0} grep {$_ != 0} @$factors
           } 1 .. $limit-1;
}

1;
