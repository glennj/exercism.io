# using algorithm from ILoveMuffins's python solution:
# https://exercism.io/tracks/python/exercises/palindrome-products/solutions/d68cab86cad94d4d821f26da44bb0722

package PalindromeProducts;

use strict;
use warnings;

use Exporter qw<import>;
our @EXPORT_OK = qw<smallest_palindrome largest_palindrome>;

use Carp;
use POSIX qw/ floor /;

sub smallest_palindrome {
    my ( $min, $max ) = @_;
    croak "min must be <= max" unless $min <= $max;
    return results(
        $min * $min,
        $max * $max,
        1,
        $min,
        sub {return (shift) <= (shift)},
        sub {my $n = shift; $min <= $n && $n <= $max}
    );
}

sub largest_palindrome {
    my ( $min, $max ) = @_;
    croak "min must be <= max" unless $min <= $max;
    return results(
        $max * $max,
        $min * $min,
        -1,
        $max,
        sub {return (shift) >= (shift)},
        sub {my $n = shift; $min <= $n && $n <= $max}
    );
}

sub is_palindrome {
    my $input = shift;
    return "".$input eq reverse("".$input);
}

sub results {
    my ($product_start, $product_end, $step, $factor_start, $cmp, $in_range) = @_;
    my $result = {};

    for (my $product = $product_start; $cmp->($product, $product_end); $product += $step) {
        if (is_palindrome $product) {

            my @factors = ();
            for (my $factor = $factor_start; $cmp->($factor * $factor, $product); $factor += $step) {
                my $fact2 = floor($product / $factor);
                if ($factor * $fact2 == $product && $in_range->($fact2)) {
                    push @factors, [sort {$a <=> $b} $factor, $fact2];
                }
            }

            if (@factors > 0) {
                return {
                    factors => \@factors,
                    value => $product
                };
            }
        }
    }

    return {factors => [], value => undef};
}

1;
