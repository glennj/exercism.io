package SumOfMultiples;

use v5.38;

use Exporter qw/ import /;
our @EXPORT_OK = qw/ sum_of_multiples /;

use List::Util  qw/ any sum0 /;

sub sum_of_multiples($factors, $limit) {
    return sum0 
           grep {
               my $n = $_;
               any {$n % $_ == 0} grep {$_ != 0} @$factors
           }
           1 .. $limit-1;
}

1;
