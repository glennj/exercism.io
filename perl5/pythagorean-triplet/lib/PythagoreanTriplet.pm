package PythagoreanTriplet;

use v5.38;

use Exporter qw<import>;
our @EXPORT_OK = qw<triplets_with_sum>;

sub triplets_with_sum ($sum) {
    my @triplets = ();
    my ($a, $b, $c);
    while (1) {
        $a++;
        my $num = $sum * ($sum - 2 * $a);
        my $den = 2 * ($sum - $a);
        my $b = int($num / $den);
        last if $b < $a;
        next if $num % $den != 0;
        $c = $sum - $a - $b;
        push @triplets, [$a, $b, $c];
    }
    return \@triplets;
}
