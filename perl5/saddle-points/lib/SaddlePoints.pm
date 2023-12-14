package SaddlePoints;

use v5.38;

use Exporter qw<import>;
our @EXPORT_OK = qw<saddle_points>;

# import my previous solution
use lib '.';
use Matrix;

sub saddle_points ($matrix) {
    return [
        map {{row => 1 + $_->[0], column => 1 + $_->[1]}}
            Matrix->new($matrix)->saddle_points->@*
    ];
}
