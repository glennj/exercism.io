package CollatzConjecture;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<steps>;

sub even ($number) { $number % 2 == 0; }

sub steps ($number) {
    die "Only positive integers are allowed" if $number < 1;

    my $steps = 0;
    while ($number > 1) {
        $number = even($number) ? $number / 2 : $number * 3 + 1;
        $steps++;
    }
    return $steps;
}

1;
