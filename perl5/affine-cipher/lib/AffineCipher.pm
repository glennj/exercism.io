package AffineCipher;

## no critic (ValuesAndExpressions::ProhibitVersionStrings)
## no critic (RegularExpressions::RequireExtendedFormatting)

use v5.42;
use Readonly;
use Carp;

use Exporter qw<import>;
our @EXPORT_OK = qw<encode decode>;

Readonly my $m => 26;
Readonly my $A => ord 'a';

sub to_num  ($char) { return ord($char) - $A }
sub to_char ($num)  { return chr($num + $A) }

sub mmi ($x, $m) {
    foreach my $i (1..$m) {
        return $i if ($x * $i) % $m == 1;
    }
    croak "Cannot find MMI of $x and $m";
}

sub gcd ($x, $y) { return $y == 0 ? $x : gcd($y, $x % $y) }

sub spaces ($s) { return join ' ', ($s =~ /(.{1,5})/g) }

sub encrypt ($text, $fn) {
    my $tidy = lc($text) =~ s/\W//gr;
    return $tidy =~ s{([a-z])}{ $fn->($1) }ger;
}

sub encode ($phrase, $A, $B) {
    croak 'a and m must be coprime.' unless gcd($A, $m) == 1;
    return spaces encrypt $phrase, sub ($char) {
        to_char(($A * to_num($char) + $B) % $m)
    };
}

sub decode ($phrase, $A, $B) {
    croak 'a and m must be coprime.' unless gcd($A, $m) == 1;
    my $A_inv = mmi $A, $m;
    return encrypt $phrase, sub ($char) {
        to_char(($A_inv * (to_num($char) - $B)) % $m)
    };
}

1;
