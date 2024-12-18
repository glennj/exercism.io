package EliudsEggs;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<egg_count>;

sub egg_count ($number, $count = 0) {
    return $count if $number == 0;
    return egg_count($number >> 1, $count + ($number & 1));
}

1;
