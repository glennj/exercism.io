## no critic (RegularExpressions::RequireExtendedFormatting)

package CryptoSquare;

use 5.024;
#use strictures  2;
use strict;
use warnings;

use POSIX       qw/ ceil /;
use List::Util  qw/ reduce /;
#use Exporter::Easiest 'OK => cipher';
use Exporter qw<import>;
our @EXPORT_OK = qw<cipher>;

sub cipher {
    my ($plaintext) = @_;
    return '' if $plaintext eq '';

    my $normalized = lc($plaintext) =~ s/\W//gr;
    my $size = ceil sqrt length $normalized;
    my @plain_segments = map {[split '']} $normalized =~ /(.{1,$size})/g;

    # transpose the "rows" of the plain_segments into "columns"
    my $cipher_segments = reduce {
        $a->[$b] = join '', map {$_->[$b] // ' '} @plain_segments;
        $a;
    } [], (0 .. $size - 1);

    return join ' ', @$cipher_segments;
}

1;
